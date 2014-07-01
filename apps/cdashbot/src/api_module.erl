-module(api_module).
-export([list_gen/0, list_id_gen/0,  id_tuple_gen/1, describe_gen_id/1, 
		list_gen_rexp/1, check_active/1, ver_gen/0, count_builds/1,
		count_builds_week/1, list_string_gen/0, shedule_start/1, 
		status/0, status/1, status_string/1, site_list/0, site/1]).
-include_lib("cdashbot_wrk.hrl").
%%----------------------------------------------------------------------------------------------
%% Exported Function
%%----------------------------------------------------------------------------------------------
%% Проверяем и генерируем список проектов.

list_gen() ->
	case string:equal(?PLIST, "all")  of
		true -> 
			Url = ?URL ++ ?API_LIST,
			case check_status(Url) of 
				[true, Body] -> 
						[erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, 
						lists:append(proplists:get_value(<<"projects">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))))];
				[false, Body] -> 
					error_text(Body)
			end;
		false -> [?PLIST]
	end.
%% Генерируем первоначальный список для монитора
list_id_gen() ->  
	lists:map(fun(X) -> id_tuple_gen(X) end, list_gen()).
%% Генерируем кортеж для ets для монитора
id_tuple_gen(Project) ->
	Url = ?URL ++ ?API_BL ++ Project ++ ?API_CN10,
	case check_status(Url) of
		[true, Body] ->
			List = lists:append(proplists:get_value(<<"builds">>, 
				jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Id_list = proplists:get_all_values(<<"id">>, List), 
			Id = lists:map(fun(X) -> erlang:integer_to_list(X) end, Id_list), 
			{Project, Id};
		[false, Body] -> 
			cdashbot_wrk:send(error_text(Body))
	end.

	
%% Генерируем строку сообщения	
list_string_gen() -> 
	Url = ?URL ++ ?API_LIST,
	case check_status(Url) of
		[true, _] ->
			string:join(lists:map(fun(X) -> describe_gen(X) end, list_gen()), " "); 

		[false, Body] ->
			error_text(Body)
	end.
	

%% Проверяем и генерируем список проектов согласно Regexp.

list_gen_rexp(Rexp) -> 
	case string:equal(?PLIST, "all") of
	true  -> 
		Url = ?URL ++ ?API_LIST,
		Plist = case check_status(Url) of
			[true, Body] -> 
				List = lists:append(proplists:get_value(<<"projects">>, 
					jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
					proplists:get_all_values(<<"name">>, List);
			[_, Body] -> error_text(Body)
		end;
	false -> 
		Plist = lists:map(fun(U) -> erlang:list_to_binary(U) end, string:tokens(?PLIST, ", "))
	end,
	{ok, Rexpc} = re:compile(Rexp, [unicode, caseless]), 
		Filt = fun(Y) ->
		nomatch =/= re:run(Y, Rexpc, [global])
			end,
	lists:map(fun(Z) -> describe_gen(erlang:binary_to_list(Z)) end, 
			lists:filter(Filt, Plist)). 

%%  Проверяем проект на активность.
check_active(Rexp) -> 
	Url = ?URL ++ ?API_BL ++ Rexp ++ ?API_CN,
	case check_status(Url) of 
		[true, Body] ->  
			List = lists:append(proplists:get_value(<<"builds">>, 
					jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			{Date, _Time} = get_date_time(List),
			Id = erlang:integer_to_list(proplists:get_value(<<"id">>, List)), 
			Count = calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days(Date),
			case  Count =< 7 of
				true -> 
					case Count =< 3 of 
						true -> active(Rexp);
						_ -> active10(Rexp)
					end;
				_ -> inactive(Id, Count) 
					
			end;
		[true, []] -> 
			cdashbot_wrk:send(io_lib:format("No such project: ~s~n", [Rexp]));
		[_, Body] -> 
			error_text(Body)
	end.

%% Генерируем строку версии 
ver_gen() ->
	Url = ?URL ++ ?API_VER,
	case check_status(Url) of 
		[true, Body] -> 
			List = erlang:binary_to_list(proplists:get_value(<<"version">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))), 
			io_lib:format("CDash version: ~s~n", [List]);
		[_, Body] -> 
			error_text(Body)
	end.

%%----------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------

%% Проверяем статус в Json
check_status(Url) ->
			try httpc:request(Url) of
				{ok, {_, _, Body}} ->
					try jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]) of
						{error,_,_} -> 
							lager:info("Wrong Json: ~s~n", [Body]),
							check_status(Url);
						Jlist ->
							Status = proplists:get_value(<<"status">>, Jlist),
							[Status, Body]

					catch 
						_:_ -> ok
					end;
				{error,_} ->
					lager:info("Server is not available ~s", [Url]),
					check_status(Url)
			catch
				_:_ -> ok
			end.


inactive(Id, Count) ->
	Url = ?URL ++ ?API_DI ++ Id,
	case check_status(Url) of 
		[true, Body] -> 
			List = lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Name = proplists:get_value(<<"project">>, List),
			io_lib:format("Project ~s is inactive (no builds for more than ~s days)",
      												[Name, erlang:integer_to_list(Count)]),
			"Last build" ++ describe_gen_id(erlang:list_to_integer(Id));
		[_, Body] -> 
			error_text(Body)
	end.
%% Считаем количество сборок за неделю у активного проекта и генерируем сообщение в зависимости от количества
active(Rexp) ->
	Count = count_builds(Rexp),
	case  Count =< 2 of
		true -> 
			active10(Rexp);
		false -> 	
			string:join([io_lib:format("~s builds for project ~s last 3 days ~n", 
										[erlang:integer_to_list(Count),
										Rexp]) ++ lists:concat(builds_select(Rexp, 3))], "")
	end.

active10(Rexp) ->
	Count = count_builds_week(Rexp),
	io_lib:format("~s builds for project ~s last week ~n", 
						[erlang:integer_to_list(Count),
						 Rexp]) ++ lists:concat(builds_select(Rexp, 7)).
%% Генерируем описание проекта
describe_gen(Name) -> 
	Url = ?URL ++ ?API_DESC ++ Name,
	case check_status(Url) of 
		[true, Body] -> 
			List = lists:append(proplists:get_value(<<"projects">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			NamP = proplists:get_value(<<"name">>, List),
			DescP = proplists:get_value(<<"description">>, List),
			io_lib:format("~s - ~s. ~n", [NamP, DescP]);
		[_, Body] ->    
			error_text(Body)
	end.
%%Генерируем суммари  по проекту.
describe_gen_id(Id) ->
	Url = ?URL ++ ?API_DI ++ erlang:integer_to_list(Id),
	case check_status(Url) of 
		[true, Body] -> 
			List = lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Err = proplists:get_value(<<"n_errors">>, List),
			Warn = proplists:get_value(<<"n_warnings">>, List),
			TestP = proplists:get_value(<<"n_test_pass">>, List),
			TestF = proplists:get_value(<<"n_test_fail">>, List),
			TestN = proplists:get_value(<<"n_test_not_run">>, List),
			Site = proplists:get_value(<<"site">>, List),
			Name = proplists:get_value(<<"name">>, List),
			TestS = TestP + TestF + TestN,
			case {Err, Warn, TestS} of 
				{0, Warn, TestS} when Warn > 0, TestS > 0 -> 
									io_lib:format(" on ~s: ~s. Warnings: ~s. Tests passed: ~s/~s.~n",
																					[Site, 
						 															 Name,
						 									 erlang:integer_to_list(Warn),
						 									erlang:integer_to_list(TestP),
						 								  erlang:integer_to_list(TestS)]);
				{0, 0, TestS} when TestS > 0 ->   io_lib:format(" on ~s: ~s. Tests passed: ~s/~s.~n",
																					[Site, 
						 															 Name,
						 									erlang:integer_to_list(TestP),
						 								  erlang:integer_to_list(TestS)]);
				{0, Warn, 0} when Warn > 0 ->	io_lib:format(" on ~s: ~s. Build Warnings: ~s.~n", 
																					[Site, 
						 															 Name,
						 								   erlang:integer_to_list(Warn)]);
				{Err, 0, _} when Err > 0 -> io_lib:format(" on ~s: ~s, failed. Build Errors: ~s.~n", 
																					[Site, 
						 															 Name,
						 									erlang:integer_to_list(Err)]);
				{Err, Warn, _} when Err > 0, Warn >0 -> io_lib:format(" on ~s: ~s, failed. Build Errors: ~s. Build Warnings: ~s.~n", 
																					[Site, 
						 															 Name,
						 									  erlang:integer_to_list(Err),
						 								   erlang:integer_to_list(Warn)]);
				{0, 0, 0} -> io_lib:format(" on ~s: ~s. All good.~n",
																					[Site, 
																					Name]) 
			end;
		[_, Body] -> error_text(Body)
	end.

%build_describe_id() -> 
	
%% Генерация ошибку и Json
error_text(Body) ->
			ErrText = proplists:get_value(<<"message">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}])),
			io_lib:format("Error: ~s", [erlang:binary_to_list(ErrText)]).

count_builds(Rexp) -> 
	Url = ?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(3),
	case check_status(Url) of
		[true, Body] ->
			erlang:length(proplists:get_all_values(<<"id">>, 
			lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}])))));
		[_, Body] ->
			lager:info("~s", Body)
	end.

count_builds_week(Rexp) ->  
	Url = ?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(7),
	case check_status(Url) of 
		[true, Body] ->
			erlang:length(proplists:get_all_values(<<"id">>, 
			lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}])))));
		[_, Body] ->
			lager:info("~s", Body)
	end.

day_range(Count) -> 
	StringToday = string:join(io_lib:format("~w~w~w", erlang:tuple_to_list(erlang:date())), "-"),
	ThreeDay = string:join(io_lib:format("~w~w~w", erlang:tuple_to_list(
							calendar:gregorian_days_to_date((
								calendar:date_to_gregorian_days(erlang:date()) - Count)))), "-"),
	string:join([ThreeDay, StringToday], ",").

builds_select(Rexp, Count) ->
	Url = ?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(Count),
	case check_status(Url) of 
		[true, Body] ->
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			List = lists:append(proplists:get_value(<<"builds">>, Jlist)),
			Id = proplists:get_all_values(<<"id">>, List),
			Name = proplists:get_all_values(<<"name">>, List),
			Site = proplists:get_all_values(<<"site">>, List),
			Zlist = lists:zip(Name, lists:zip(Site, Id)),
			Blist = lists:map(fun(Y) -> 
				lists:map(fun(X) -> 
					proplists:get_value(X, proplists:get_all_values(Y, Zlist)) end, 
						lists:usort(Site)) end, 
							lists:usort(Name)),
			lists:map(fun(X) -> "Last build" ++ describe_gen_id(X) end, lists:append(Blist));
		[_, Body] -> error_text(Body)
	end.
shedule_start(Rexp) -> 
	[PName | Options] = Rexp,
	  try make_string(Options) of 	
		Plist -> 
			Url = ?URL ++ ?API_LOGIN ++ PName ++ ?API_LOGIN_TOKEN ++ config:get_keys(PName),
			lager:info("Url: ~s~n", [Url]),
				case check_status(Url) of 
					[true, Body] ->
						Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
						Token = erlang:binary_to_list(proplists:get_value(<<"token">>, Jlist)),
						Get_Url	= ?URL ++ ?API_SHED_ADD ++ PName ++ ?API_SHED_TOKEN ++ Token ++ ?API_SHED_USER ++ config:get_value(cdash_user)  ++ Plist,
						lager:info("Get_Url: ~s~n", [Get_Url]),
						case check_status(Get_Url) of 
							[true, NBody] -> 
								NJlist = jsonx:decode(erlang:list_to_binary(NBody),  [{format, proplist}]),
								Id = proplists:get_value(<<"id">>, NJlist), 
								io_lib:format("New build of ~s has been scheduled (id = ~s)", [PName, 
																							   erlang:binary_to_list(Id)]);
							[_, NBody] -> 
								error_text(NBody)
						end;	   
					[_, Body] -> 
						error_text(Body)
				end
		catch
		[error, Key] -> 
			cdashbot_wrk:send(io_lib:format("Wrong param: ~s~n", [string:sub_string(Key, 2)]))
		end. 
			

make_string([Key, Value | List]) -> 
	case [string:sub_string(Key, 2), Value] of
		["type", "exp"] -> 
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "0"])) | make_string(List)];
		["type", "nighty"] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "1"])) | make_string(List)];
		["type", "cont"] -> 
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "2"])) | make_string(List)];
		["repo", _] ->	
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["branch", _] -> 
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["tag", _] -> 
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["conf", "dbg"] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "0"])) | make_string(List)];
		["conf", "rel"] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "1"])) | make_string(List)];			
		["conf", "reldbg"] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "2"])) | make_string(List)];						
		["conf", "minsize"] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", "3"])) | make_string(List)];
		["compiler", _] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["library", _]	->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["site", _] ->
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];
		["os", _] -> 
			[lists:concat(io_lib:format("~s~s~s~s", ["&", string:sub_string(Key, 2), "=", Value])) | make_string(List)];									
		[_, _] -> 
			throw([error, Key])
		
	end;
make_string([Key])-> throw([error, Key]); 	
make_string([])-> [].


status() ->
	Url = ?URL ++ ?API_STATUS_LIST,
	case check_status(Url) of 
		[true, Body] -> 
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			List = lists:append(proplists:get_value(<<"schedules">>, Jlist)),
			status(proplists:get_all_values(<<"id">>, List));
		[_, Body] -> error_text(Body)
	end.

status(Rexp) -> 
	lists:map(fun(X) -> status_string(
		case is_integer(X) of 
			true -> 
				X;
			false ->
				erlang:list_to_integer(X)
		end) end, Rexp). 

status_string(Id) ->
	Url = ?URL ++ ?API_STATUS_DESCRIBE ++ erlang:integer_to_list(Id),
	lager:info("~s", [Url]),
	case check_status(Url) of 
		[true, Body] -> 
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			%lager:info("~s~n", [Jlist]),
			SList = lists:append(proplists:get_value(<<"schedule">>, Jlist)),
			Status = proplists:get_value(<<"status">>, SList),
			Site =  proplists:get_value(<<"site">>, SList),
			Project = proplists:get_value(<<"project">>, SList),
			case Status of 
				"scheduled" -> 
					io_lib:format("Build of ~s is ~s~n", 
														[Project,
														 Status]);
				_ -> 
					io_lib:format("Build of ~s is ~s on ~s~n", [Project,
																Status,
																Site])
			end;
		[_, Body] -> error_text(Body)
	end.

site_list() ->
	Url = ?URL ++ ?API_SITE,
	case check_status(Url) of 
		[true, Body] -> 
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			Slist = lists:sort(lists:map(fun(X) -> erlang:binary_to_list(X) end, 
						proplists:get_all_values(<<"name">>, 
							lists:append(proplists:get_value(<<"sites">>, 
											Jlist))))),
			Online = lists:map(fun(X) -> site_describe(X) ++ "online" ++ io_lib:nl() end,
						lists:filter(fun(X) -> site_status(X) =< 300 end, Slist)),
			Offline = lists:map(fun(X) -> site_describe(X) ++ "offline" ++io_lib:nl() end,
						lists:filter(fun(X) -> site_status(X) > 300 end, Slist)),
			Online ++ Offline;  

		[_, Body] -> error_text(Body)
	end.

site(Rexp) -> 
	Url = ?URL ++ ?API_SITE,
	case check_status(Url) of 
		[true, Body] -> 
			{ok, Rexpc} = re:compile(Rexp, [unicode, caseless]), 
			Filt = fun(Y) ->
			nomatch =/= re:run(Y, Rexpc, [global])
					end,
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			Slist = lists:map(fun(X) -> erlang:binary_to_list(X) end, 
						lists:filter(Filt, 
							lists:sort(proplists:get_all_values(<<"name">>, 
											lists:append(proplists:get_value(<<"sites">>, 
													Jlist)))))),
			Online = lists:map(fun(X) -> site_describe(X) ++ "online" ++ io_lib:nl() end,
						lists:filter(fun(X) -> site_status(X) =< 300 end, Slist)),
			Offline = lists:map(fun(X) -> site_describe(X) ++ "offline" ++ io_lib:nl() end,
						lists:filter(fun(X) -> site_status(X) > 300 end, Slist)),
			Online ++ Offline;
		[_, Body] -> error_text(Body)
	end.
	


site_status(Site) ->
	Url = ?URL ++ ?API_SITE_DESCRIBE ++ Site,
	case check_status(Url) of
		[true, Body] -> 
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			Slist = proplists:get_value(<<"last_ping">>, lists:append(proplists:get_value(<<"sites">>, 
									Jlist))),
			
			
			NowS = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(erlang:now())),
			NowS - calendar:datetime_to_gregorian_seconds(get_date_time(Slist));
 		[_, Body] -> error_text(Body)
	end.

site_describe(Site) ->
	Url = ?URL ++ ?API_SITE_DESCRIBE ++ Site,
	case check_status(Url) of
		[true, Body] -> 
			Jlist = jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]),
			Slist = lists:append(proplists:get_value(<<"sites">>, 
									Jlist)),
			System = erlang:binary_to_list(proplists:get_value(<<"system_name">>, Slist)),
			Os = erlang:binary_to_list(proplists:get_value(<<"os_name">>, Slist)),			 
			io_lib:format("~s: ~s (~s) - ", [Site,
										    System,
										    Os]);
		[_, Body] -> error_text(Body)
	end.

get_date_time(Plist) -> 
	{{erlang:binary_to_integer(proplists:get_value(<<"year">>, Plist)),
	 erlang:binary_to_integer(proplists:get_value(<<"month">>, Plist)),
	 erlang:binary_to_integer(proplists:get_value(<<"day">>, Plist))},
   	erlang:list_to_tuple(
				lists:map(fun(X) -> erlang:list_to_integer(X) end, 
					string:tokens(erlang:binary_to_list(
						proplists:get_value(<<"time">>, Plist)),":")))}.


-module(api_module).
-export([list_gen/0, list_id_gen/0,  id_tuple_gen/1, describe_gen_id/1, list_gen_rexp/2, check_active/1, ver_gen/0, count_builds/1,count_builds_week/1, list_string_gen/0]).
-include_lib("cdashbot_wrk.hrl").
%%----------------------------------------------------------------------------------------------
%% Exported Function
%%----------------------------------------------------------------------------------------------
%% Проверяем и генерируем список проектов.

list_gen() ->
	case string:equal(?PLIST, "all")  of
		true -> 
			{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
			case check_status(Body) of 
				true -> 
						[erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, 
						lists:append(proplists:get_value(<<"projects">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))))];
				_ -> 
					error_text(Body)
			end;
		false -> [?PLIST]
	end.
%% Генерируем первоначальный список для монитора
list_id_gen() ->  
	lists:map(fun(X) -> id_tuple_gen(X) end, list_gen()).

id_tuple_gen(Project) ->
	{ok, {_, _, Body}} =  httpc:request(?URL ++ ?API_BL ++ Project ++ ?API_CN10),
	case check_status(Body) of
		true ->
			List = lists:append(proplists:get_value(<<"builds">>, 
				jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Id_list = proplists:get_all_values(<<"id">>, List), 
			Id = lists:map(fun(X) -> erlang:integer_to_list(X) end, Id_list), 
			{Project, Id};
		false -> 
			cdashbot_wrk:send(error_text)
	end.

	
%% Генерируем строку сообщения	
list_string_gen() -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
	case check_status(Body) of
		true ->
			string:join(lists:map(fun(X) -> describe_gen(X) end, list_gen()), " "); 

		_ ->
			error_text(Body)
	end.
	

%% Проверяем и генерируем список проектов согласно Regexp.

list_gen_rexp(Jlist, Rexp) -> 
	case check_status(Jlist) of
		true -> 
			{ok, Rexpc} = re:compile(Rexp, [unicode, caseless]), 
			Filt = fun(X) ->
				nomatch =/= re:run(X, Rexpc, [global])
				end,
			List = lists:filter(Filt, [erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, 
			lists:append(proplists:get_value(<<"projects">>, jsonx:decode(erlang:list_to_binary(Jlist),  [{format, proplist}]))))]),
			string:join(lists:map(fun(X) -> describe_gen(X) end, List), "");
		_ -> 
			error_text(Jlist)
	end.	

check_active(Rexp) -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_BL ++ Rexp ++ ?API_CN),
	case check_status(Body) of 
		true -> 
			List = lists:append(proplists:get_value(<<"builds">>, 
					jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Year = erlang:binary_to_integer(proplists:get_value(<<"year">>, List)),
			Month = erlang:binary_to_integer(proplists:get_value(<<"month">>, List)),
			Day = erlang:binary_to_integer(proplists:get_value(<<"day">>, List)),
			Id = erlang:integer_to_list(proplists:get_value(<<"id">>, List)), 
			Count = calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days({Year, Month, Day}),
			case  Count =< 7 of
				true -> 
					case Count =< 3 of 
						true -> active(Rexp);
						_ -> active10(Rexp)
					end;
				_ -> inactive(Id, Count) 
					
			end;
		_ -> 
			error_text(Body)
	end.

%% Генерируем строку версии 
ver_gen() ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_VER),
	case check_status(Body) of 
		true -> 
			List = erlang:binary_to_list(proplists:get_value(<<"version">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))), 
			io_lib:format("Cdash version: ~s~n", [List]);
		_ -> 
			error_text(Body)
	end.

%%----------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------

%% Проверяем статус в Json
check_status(Jlist) ->
	
			proplists:get_value(<<"status">>, jsonx:decode(erlang:list_to_binary(Jlist),  [{format, proplist}])).

inactive(Id, Count) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_DI ++ Id),
	case check_status(Body) of 
		true -> 
			List = lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			Err = proplists:get_value(<<"n_errors">>, List),
			Name = proplists:get_value(<<"project">>, List),
			Bname = proplists:get_value(<<"name">>, List),
			Sname = proplists:get_value(<<"site">>, List),
			Warn = proplists:get_value(<<"n_warnings">>, List),
			Testp = proplists:get_value(<<"n_test_pass">>, List),
			Testf = proplists:get_value(<<"n_test_fail">>, List),
			Testn = proplists:get_value(<<"n_test_not_run">>, List),
			Tests = Testp + Testf + Testn,
			case  Err > 0 of
				true -> io_lib:format("Project ~s is inactive (no builds for more than ~s days):~n Last build is ~s on ~s. Build errors: ~s, warnings: ~s~n",
      												[Name, 
      												erlang:integer_to_list(Count),
      												 Bname, Sname, 
      												 erlang:integer_to_list(Err), 
      												 erlang:integer_to_list(Warn)]);  
				_ ->  io_lib:format("Project ~s is inactive (no builds for more than ~s days):~n Last build is ~s on ~s. Build warnings: ~s. Tests passed: ~s/~s~n", 
      									[Name, 
      									erlang:integer_to_list(Count),
      									 Bname, Sname, 
      									 erlang:integer_to_list(Warn),
      									 erlang:integer_to_list(Testp),
      									 erlang:integer_to_list(Tests)]) 
			end;
		_ -> 
			error_text(Body)
	end.

active(Rexp) ->
	Count = count_builds(Rexp),
	case  Count =< 2 of
		true -> 
			active10(Rexp);
		false -> 	
			string:join([io_lib:format("~s builds for project ~s last 3 days ~n", 
										[erlang:integer_to_list(Count),
										Rexp]) ++ lists:concat(builds_select(Rexp, 2))], "")
	end.
	
active10(Rexp) ->
	Count = count_builds_week(Rexp),
	io_lib:format("~s builds for project ~s last week ~n", 
						[erlang:integer_to_list(Count),
						 Rexp]) ++ lists:concat(builds_select(Rexp, 6)).
describe_gen(Name) -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_DESC ++ Name),
	case check_status(Body) of 
		true -> 
			List = lists:append(proplists:get_value(<<"projects">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
			NamP = proplists:get_value(<<"name">>, List),
			DescP = proplists:get_value(<<"description">>, List),
			io_lib:format("~s - ~s. ~n", [NamP, DescP]);
		_ ->    
			error_text(Body)
	end.
describe_gen_id(Id) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_DI ++ erlang:integer_to_list(Id)),
	case check_status(Body) of 
		true -> 
			List = lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))),
%			Err = proplists:get_value(<<"n_errors">>, List),
%			Warn = proplists:get_value(<<"n_warnings">>, List),
			TestP = proplists:get_value(<<"n_test_pass">>, List),
			TestF = proplists:get_value(<<"n_test_fail">>, List),
			TestN = proplists:get_value(<<"n_test_not_run">>, List),
			Site = proplists:get_value(<<"site">>, List),
			Name = proplists:get_value(<<"name">>, List),
			TestS = TestP + TestF + TestN,
			io_lib:format("Last build on ~s: ~s. Tests passed: ~s/~s ~n", 
															[erlang:binary_to_list(Site),
															erlang:binary_to_list(Name),
															erlang:integer_to_list(TestP),
															erlang:integer_to_list(TestS)]);
		_ -> error_text(Body)
	end.

error_text(Body) ->
			ErrText = proplists:get_value(<<"message">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}])),
			io_lib:format("Error: ~s", ErrText).

count_builds(Rexp) -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(2)),
	erlang:length(proplists:get_all_values(<<"id">>, 
		lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))))).

count_builds_week(Rexp) ->  
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(6)),
	erlang:length(proplists:get_all_values(<<"id">>, 
		lists:append(proplists:get_value(<<"builds">>, jsonx:decode(erlang:list_to_binary(Body),  [{format, proplist}]))))).

day_range(Count) -> 
	StringToday = string:join(io_lib:format("~w~w~w", erlang:tuple_to_list(erlang:date())), "-"),
	ThreeDay = string:join(io_lib:format("~w~w~w", erlang:tuple_to_list(
							calendar:gregorian_days_to_date((
								calendar:date_to_gregorian_days(erlang:date()) - Count)))), "-"),
	string:join([ThreeDay, StringToday], ",").

builds_select(Rexp, Count) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_BL ++ Rexp ++ ?API_DN ++ day_range(Count)),
	case check_status(Body) of 
		true ->
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
			lists:map(fun(X) -> describe_gen_id(X) end, lists:append(Blist));
		_ -> error_text(Body)
	end.

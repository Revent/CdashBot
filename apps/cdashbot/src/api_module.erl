-module(api_module).
-export([list_gen/1, list_gen_rexp/2, check_active/1, ver_gen/0]).
-include_lib("cdashbot_wrk.hrl").

%%----------------------------------------------------------------------------------------------
%% Exported Function
%%----------------------------------------------------------------------------------------------
%% Проверяем и генерируем списо проектов.

list_gen(Jlist) ->
	case check_status(Jlist) of 
		true -> 
			List = [erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, 
			lists:append(proplists:get_value(<<"projects">>, jsx:decode(erlang:list_to_binary(Jlist)))))],
			string:join(lists:map(fun(X) -> describe_gen(X) end, List), ""); 

		_ -> "Error: " ++ proplists:get_value(<<"message">>, jsx:decode(erlang:list_to_binary(Jlist)))
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
			lists:append(proplists:get_value(<<"projects">>, jsx:decode(erlang:list_to_binary(Jlist)))))]),
			string:join(lists:map(fun(X) -> describe_gen(X) end, List), "");
		_ -> 
			"Error: " ++ proplists:get_value(<<"message">>, jsx:decode(erlang:list_to_binary(Jlist)))
	end.	

check_active(Rexp) -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_BL ++ Rexp ++ ?API_CN),
	case check_status(Body) of 
		true -> 
			List = lists:append(proplists:get_value(<<"builds">>, 
					jsx:decode(erlang:list_to_binary(Body)))),
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
		_ -> "Error: " ++ proplists:get_value(<<"message">>, jsx:decode(erlang:list_to_binary(Body)))
	end.

%% Генерируем строку версии 
ver_gen() ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?VER),
	io_lib:format("Cdash version: ~s~n", [Body]).

%%----------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------

%% Проверяем статус в Json
check_status(Jlist) ->
	proplists:get_value(<<"status">>, jsx:decode(erlang:list_to_binary(Jlist))).


inactive(Id, Count) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_DI ++ Id),
	case check_status(Body) of 
		true -> 
			List = jsx:decode(erlang:list_to_binary(Body)),
			Err = proplists:get_value(<<"n_errors">>, List),
			Name = proplists:get_value(<<"name">>, List),
			Bname = proplists:get_value(<<"name">>, List),
			Sname = proplists:get_value(<<"site">>, List),
			Warn = proplists:get_value(<<"n_warnings">>, List),
			Testp = proplists:get_value(<<"n_test_pass">>, List),
			Testf = proplists:get_value(<<"n_test_fail">>, List),
			Testn = proplists:get_value(<<"n_test_not_run">>, List),
			Tests = Testp + Testf + Testn,
			case  Err > 0 of
				true -> io_lib:format("Project ~s is inactive (no builds for more than ~s)~n Last build is ~s on ~s.~n Build errors: ~s, warnings: ~s", 
      												[Name, 
      												erlang:integer_to_list(Count),
      												 Bname, Sname, 
      												 erlang:integer_to_list(Err), 
      												 erlang:integer_to_list(Warn)]);  
				_ ->  io_lib:format("Project ~s is inactive (no builds for more than ~s)~n Last build is ~s on ~s.~n Build warnings: ~s.~n Tests passed: ~s/~s", 
      									[Name, 
      									erlang:integer_to_list(Count),
      									 Bname, Sname, 
      									 erlang:integer_to_list(Warn),
      									 erlang:integer_to_list(Testp),
      									 erlang:integer_to_list(Tests)]) 
			end;
		_ -> "Error: " ++ proplists:get_value(<<"message">>, jsx:decode(erlang:list_to_binary(Body)))
	end.

active(Rexp) ->
	 io_lib:format("~s active", [Rexp]).
active10(Rexp) ->
	io_lib:format("~s 10 Last", [Rexp]).
describe_gen(Name) -> 
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_DESC ++ Name),
	case check_status(Body) of 
		true -> 
			List = jsx:decode(erlang:list_to_binary(Body)),
			NamP = proplists:get_value(<<"name">>, List),
			DescP = proplists:get_value(<<"description">>, List),
			io_lib:format("~s - ~s. ~n", [NamP, DescP]);
		_ ->    
			"Error: " ++ proplists:get_value(<<"message">>, jsx:decode(erlang:list_to_binary(Body)))
	end.

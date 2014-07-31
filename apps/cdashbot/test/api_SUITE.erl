-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        % список тестов
        json_validate_test_true,
        json_validate_test_false,
        json_validate_test_error,
        check_status_test_error
    ].

init_per_suite(Config) ->
    % действия, выполняемые перед запуском набора тестов
    
    Config.

init_per_testcase(_, Config) ->
    % действия, выполняемые перед запуском теста
    Config.

end_per_testcase(_, Config) ->
    % действия, выполняемые после завершения теста
    Config.

end_per_suite(Config) ->
    % действия, выполняемые после завершения всего набора тестов
    
    Config.

json_validate_test_true(_Config) ->
    Body = "{\"status\":true,\"projects\":[{\"id\":1,\"name\":\"CDash\"},{\"id\":2,\"name\":\"CDashBot\"}]}",
    ?assertEqual([true, Body], api_module:json_validate(Body)).

json_validate_test_false(_Config) -> 
  Body = "{\"status\":false,\"message\":\"Site 'garik-laptop!' not found.\"}",
  ?assertEqual([false, Body], api_module:json_validate(Body)). 

json_validate_test_error(_Config) -> 
  Body = "not json",
  ?assertExit("not json", api_module:json_validate(Body)).

check_status_test_error(_Config) -> 
    Url = "localhost",
    ?assertExit("Server is not available", api_module:check_status(Url)).
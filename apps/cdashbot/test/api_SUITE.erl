-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    % список тестов
    json_validate_test
  ].

init_per_suite(Config) ->
  % действия, выполняемые перед запуском набора тестов
  ok = application:start(inets),
  ok = application:start(exmpp), 
  ok = application:start(cdashbot), 
  Config.

init_per_testcase(_, Config) ->
  % действия, выполняемые перед запуском теста
  Config.

end_per_testcase(_, Config) ->
  % действия, выполняемые после завершения теста
  Config.

end_per_suite(Config) ->
  % действия, выполняемые после завершения всего набора тестов
  ok = application:stop(cdashbot),
  ok = application:stop(exmpp),
  ok = application:stop(inets),
  Config.

json_validate_test(_Config) ->
  Body = "{\"status\":true,\"projects\":[{\"id\":1,\"name\":\"CDash\"},{\"id\":2,\"name\":\"CDashBot\"}]}",
  Result = api_module:json_validate(Body),
  ?assertEqual([true, Body], Result).

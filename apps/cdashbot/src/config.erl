-module(config).
-export([get_value/1, get_keys/1]).
-include("cdashbot_wrk.hrl").
get_value(Key) ->
	{ok, Conf} = file:consult(?CONF),
	proplists:get_value(Key, Conf).

get_keys(Key) ->
	{ok, Keys} = file:consult(?KEYS),
	proplists:get_value(Key, Keys).  
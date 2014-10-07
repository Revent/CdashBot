-module(cdashbot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
	application:start(inets),
	application:start(exmpp),
	application:start(lager),
  application:start(jsonx),
	application:start(cdashbot),
	ok.

start(_StartType, _StartArgs) ->
  cdashbot_sup:start_link(cdashbot_wrk).

stop(_State) ->
	application:stop(cdasbot),
	application:stop(lager),
	application:stop(exmpp),
  application:stop(jsonx),
  application:stop(inets),
  ok.

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
	application:start(cdashbot),
	application:start(api_module), 
	ok.

start(_StartType, _StartArgs) ->
    cdashbot_sup:start_link().

stop(_State) ->
    ok.

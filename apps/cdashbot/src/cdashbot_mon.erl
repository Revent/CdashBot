-module(cdashbot_mon).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Settings
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-include_lib("cdashbot_wrk.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	ets:new(build, [named_table]),
	check_last_build(),
    Timer = erlang:send_after(5000, ?MODULE, project_loop),
	{ok, Timer}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(project_loop, OldTimer) -> 
   erlang:cancel_timer(OldTimer),
   lists:foreach(fun(X) -> diff_project_id(X) end, api_module:list_gen()),
   Timer = erlang:send_after(20000, ?MODULE, project_loop),
   {noreply, Timer}; 

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
check_last_build() -> 
	lists:foreach(fun(X) -> ets:insert(build, X) end, api_module:list_id_gen()). 

diff_project_id(Project) ->
    NewTuple = api_module:id_tuple_gen(Project),
    NewList = proplists:get_value(Project, [NewTuple]),
    List = proplists:get_value(Project, ets:lookup(build, Project)),
    
    case NewList -- List =/= [] of 
        false -> ok;
        true -> lager:info("New build: ~s", NewList -- List),
            lists:foreach(fun(X) -> 
                            cdashbot_wrk:send(api_module:describe_gen_id(erlang:list_to_integer(X))) end,
                                  NewList -- List),
            ets:insert(build, NewTuple) 
    end.




    


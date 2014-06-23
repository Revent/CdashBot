-module(cdashbot_wrk).
-behaviour(gen_server).
%% ------------------------------------------------------------------
%% Settings
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("cdashbot_wrk.hrl").

%% ------------------------------------------------------------------
%% %% Records
%% ------------------------------------------------------------------

-record( state, {session, name, queue, loop_timer = []}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, stop/0, send/1]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, process_send_packet/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

send(Message) ->
	gen_server:cast(?MODULE, {send_packet, Message}). 
%join(Room) ->
      %gen_server:cast(?MODULE, {join, Room}).

%rooms() ->
%	gen_server:cast(?MODULE, listrooms).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	process_flag(trap_exit, true),
	?MODULE ! queue_stop_timer,
	MySession = exmpp_session:start(),
	MyJID = exmpp_jid:make(?USERNAME, ?JDOMAIN, random),
	exmpp_session:auth_basic_digest(MySession, MyJID, ?PASSWORD),
	_StreamId = exmpp_session:connect_TCP(MySession, ?JSERVER, 5222),
	exmpp_session:login(MySession),
	exmpp_session:send_packet(MySession, 
		exmpp_stanza:set_recipient(exmpp_presence:available(), ?ROOM ++ "/" ++ ?NICK)),
	erlang:send_after(2000, ?MODULE, queue_loop),
	{ok, #state{session=MySession, name = ?NICK ++ ":", queue = queue:new()}}.
	
	  

handle_call(stop, _From, State) ->
	exmpp_component:stop(State#state.session),
    	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast({send_packet, Packet}, #state{queue = Q} = State) -> 
	NewState = State#state{queue = queue:in(Packet, Q)},
	{noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#received_packet{} = Packet, State) ->
	spawn_link(fun() -> process_received_packet(State, Packet) end),
	{noreply, State};

handle_info(queue_loop, State) ->
    catch erlang:cancel_timer(State#state.loop_timer),
    {Item, NewQueue} = queue:out(State#state.queue),
    case Item of
        empty -> ok;
        {value, V} -> send_packet(State#state.session, V)
    end,
    NewState = State#state{
        queue = NewQueue,
        loop_timer = erlang:send_after(1000, ?MODULE, queue_loop)},
    {noreply, NewState};

handle_info(queue_stop_timer, #state{loop_timer = LoopTimer} = State) ->
    catch erlang:cancel_timer(LoopTimer),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

process_message("ping" = Message, State) ->
	lager:info("You recieved: ~s~n", [Message]),
	process_send_packet("pong", State);

process_message("project" = Message, State) ->
	lager:info("You recieved: ~s~n", [Message]),	
	process_send_packet(api_module:list_string_gen(), State); 

process_message("help" = Message, State) ->
	{ok, Help} = ?HELP,
	lager:info("You recieved: ~s~n", [Message]),
	process_send_packet(Help, State);

process_message("version" = Message, State) ->
	lager:info("You recieved: ~s~n", [Message]),
	process_send_packet(api_module:ver_gen(), State); 

process_message("status" = Message, State) ->
	lager:info("You recieved: ~s~n", [Message]),
	process_send_packet(api_module:status(), State);

process_message("site" = Message, State) ->
	lager:info("You recieved: ~s~n", [Message]),
	process_send_packet(api_module:site_list(), State);


process_message(Message, State) ->
	process_send_packet(io_lib:format("Unknown command: ~s~n", [Message]) , State).

process_message("project" = Message, Rexp, _, State) ->
{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
lager:info("You received:  ~s ~s~n", [Message, Rexp]), 
case string:equal(?PLIST, "all")  of
		true -> List = api_module:list_gen_rexp(Body, Rexp); 
		false -> List = [?PLIST]
	end,
process_send_packet(List, State);

process_message("summary" = Message, Rexp, _, State) ->
	lager:info("You received:  ~s ~s, ~n", [Message, Rexp]),
	process_send_packet("Last build" ++ api_module:check_active(Rexp), State);

process_message("schedule" = Message, Rexp, From, State) -> 
	lager:info("You recieved: ~s ~s~n", [Message, Rexp]),
	case lists:filter(fun(X) -> string:equal(From, X) end , string:tokens(?USERS, ",")) of 
		[] ->  process_send_packet("You not autorized!", State);
		[From] -> process_send_packet(api_module:shedule_start(Rexp) , State)
	end; 

process_message("status" = Message, Rexp, _, State) ->
	lager:info("You received:  ~s ~s, ~n", [Message, Rexp]),
	process_send_packet(api_module:status(Rexp), State); 

process_message("site" = Message, Rexp, _, State) -> 
	lager:info("You received:  ~s ~s, ~n", [Message, Rexp]),
	process_send_packet(api_module:site(Rexp), State);


process_message(Message, _, _, State) ->
		process_send_packet(io_lib:format("Unknown command: ~s~n", [Message]) , State).


process_received_packet(#state{name=Name} = State, #received_packet{packet_type=message, raw_packet=Packet}) ->
	Who = erlang:binary_to_list(exmpp_stanza:get_sender(Packet)),
    Message = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'body')),
    case string:tokens(Who, "/") of 
    	[_, Nick] -> 
    		case string:str(Message, ?CONT) of
    			1 -> 
    				case string:tokens(string:sub_string(Message, 2), " ") of
    					[Cmd] ->
    						process_message(Cmd, State);
    					[Cmd | Msg] ->
    						process_message(Cmd, Msg, Nick, State);
    					_ -> 
    						ok
    				end;
    			0 ->
    				case string:tokens(Message, " ") of
						[Name | Cmd] -> 
							process_message(Cmd, State);
						[Name | [Cmd | Msg]] -> 
							process_message(Cmd, Msg, Nick, State);
						_ -> 
							ok
					end;
				_ -> ok
			end;
		_-> ok
	end;
process_received_packet(_State, _Packet) ->
	ok.

process_send_packet(Message, State) ->
	exmpp_session:send_packet(State#state.session,
				exmpp_stanza:set_recipient(exmpp_message:groupchat(Message), 
					?ROOM)). 

process_create_message(Body) ->
	From = ?USERNAME ++ "@" ++ ?JDOMAIN,
	exmpp_xml:set_attribute(
		exmpp_xml:set_attribute(
			exmpp_xml:set_attribute( 
				exmpp_message:chat(Body), <<"from">>, From), <<"to">>, ?ROOM), <<"type">>, <<"groupchat">>). 

send_packet(Session, Message) ->
	exmpp_session:send_packet(Session, process_create_message(Message)). 


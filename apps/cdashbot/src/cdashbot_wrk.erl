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
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("cdashbot_wrk.hrl").

%% ------------------------------------------------------------------
%% %% Records
%% ------------------------------------------------------------------

-record( state, {session, name, rooms=[]}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, stop/0]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, get_value/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> 
	gen_server:call(?MODULE, stop).

%join(Room) ->
      %gen_server:cast(?MODULE, {join, Room}).

%rooms() ->
%	gen_server:cast(?MODULE, listrooms).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	process_flag(trap_exit, true),
	MySession = exmpp_session:start(),
	MyJID = exmpp_jid:make(?USERNAME, ?JSERVER, random),
	exmpp_session:auth_basic_digest(MySession, MyJID, ?PASSWORD),
	_StreamId = exmpp_session:connect_TCP(MySession, ?JSERVER, 5222),
	exmpp_session:login(MySession),
	exmpp_session:send_packet(MySession, 
		exmpp_stanza:set_recipient(exmpp_presence:available(), ?ROOM ++ "/" ++ ?NICK)),
	{ok, #state{session=MySession, name = ?NICK ++ ":"}}.
	
	
	  

handle_call(stop, _From, State) ->
	exmpp_component:stop(State#state.session),
    	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#received_packet{} = Packet, State) ->
	spawn_link(fun() -> process_received_packet(State, Packet) end),
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

process_message("ping" = Message, To, State) ->
	io:format("You received: ~s: ~s~n", [To, Message]),
	case string:tokens(To, "/") of
		[Conf, Nick] ->
			exmpp_session:send_packet(State#state.session,
		  		exmpp_stanza:set_recipient(exmpp_message:groupchat(
	       			erlang:list_to_binary(Nick ++ ": pong")), Conf));
		_ ->
			ok
end;

process_message("project" = Message, To, State) ->
	io:format("You received: ~s: ~s~n", [To, Message]), 
	case string:tokens(To, "/")  of
		[Conf, _] ->
			exmpp_session:send_packet(State#state.session,
				exmpp_stanza:set_recipient(exmpp_message:groupchat(api_module:list_string_gen()),
					 Conf)); 
		_ ->
			ok
	end;
	

process_message("help" = Message, To, State) ->
	{ok, Help} = ?HELP,
	io:format("You received: ~s: ~s~n", [To, Message]),
	case string:tokens(To, "/")  of
		[Conf, _] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(Help), Conf));
		_ -> 
			ok
	end;

process_message("version" = Message, To, State) ->
io:format("You received: ~s: ~s~n", [To, Message]),
	case string:tokens(To, "/") of 
		[Conf, _] ->
			exmpp_session:send_packet(State#state.session,
				exmpp_stanza:set_recipient(exmpp_message:groupchat(api_module:ver_gen()),
				 Conf));
		_ ->
			ok 
	end; 

process_message(_, To, State) ->
	case string:tokens(To, "/") of 
		[Conf, Nick] -> 
			exmpp_session:send_packet(State#state.session, 
				exmpp_stanza:set_recipient(exmpp_message:groupchat(Nick ++ ": Чего?"),
				 Conf));
		_ ->
			ok 
	end.

process_message("project" = Message, Rexp, To, State) ->
{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
io:format("You received: ~s: ~s ~s~n", [To, Message, Rexp]), 
case string:equal(?PLIST, "all")  of
		true -> List = api_module:list_gen_rexp(Body, Rexp); 
		false -> List = [?PLIST]
	end,
case string:tokens(To, "/")  of
		[Conf, _] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(List),
			 		 Conf));
		_ ->
		ok
	end;
process_message("summary" = Message, Rexp, To, State) ->
	io:format("You received: ~s: ~s ~s~n", [To, Message, Rexp]),
	case string:tokens(To, "/")  of
		[Conf, _] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(api_module:check_active(Rexp)),
			 		 Conf));
		_ ->
			ok
	end;

process_message(_, _, To, State) ->
	case string:tokens(To, "/") of 
		[Conf, Nick] -> 
			exmpp_session:send_packet(State#state.session, 
				exmpp_stanza:set_recipient(exmpp_message:groupchat(Nick ++ ": Чего?"),
				 Conf));
		_ ->
			ok 
	end.

get_value(Key, Section) ->
	{ok, Conf} = zucchini:parse_file(?CONF),
	proplists:get_value(Key, proplists:get_value(Section, Conf)).
		


process_received_packet(#state{name=Name} = State, #received_packet{packet_type=message, raw_packet=Packet}) ->
	From = exmpp_stanza:get_sender(Packet),
    Message = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'body')),
    case string:str(Message, ?CONT) of
    	1 -> 
    		case string:tokens(Message, " ") of
    			[Msg] ->
    				process_message(string:sub_string(Msg, 2), erlang:binary_to_list(From), State);
    			[Msg, Rexp] ->
    				process_message(string:sub_string(Msg, 2), Rexp, erlang:binary_to_list(From), State);
    			_ -> 
    				ok
    		end;
    	0 ->
    		case string:tokens(Message, " ") of
				[Name, Msg] -> 
					process_message(Msg, erlang:binary_to_list(From), State); 
				[Name, Msg, Rexp] -> 
					process_message(Msg, Rexp, erlang:binary_to_list(From), State);
				_ -> 
					ok
			end;
		_ -> ok
	end;

process_received_packet(_State, _Packet) ->
	ok.
 
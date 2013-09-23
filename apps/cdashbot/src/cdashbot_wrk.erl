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
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
	io:format("You received: ~s: ~s~n", [To, Message]), 
	List = [ erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, lists:append(jsx:decode(erlang:list_to_binary(Body))))],
	case string:tokens(To, "/")  of
		[Conf, Nick] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(Nick ++ ": " ++ string:join(List, ", ")),
			 		 Conf)); 
	_ ->
		ok
end;
process_message("help" = Message, To, State) ->
	{ok, Help} = ?HELP,
	io:format("You received: ~s: ~s~n", [To, Message]),
	case string:tokens(To, "/")  of
		[Conf, Nick] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(Help), Conf));
		_ -> 
			ok
	end;


process_message(_Message, _To, _State) -> 
	ok.

process_message("project" = Message, Rexp, To, State) ->
{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_LIST),
{ok, Rexpc} = re:compile(Rexp, [unicode, caseless]), 
Filt = fun(X) ->
		nomatch =/= re:run(X, Rexpc, [global])
		end, 
io:format("You received: ~s: ~s ~s~n", [To, Message, Rexp]), 
	List = [ erlang:binary_to_list(X) || X <- proplists:el (<<"name">>, lists:append(jsx:decode(erlang:list_to_binary(Body))))],
	case string:tokens(To, "/")  of
		[Conf, Nick] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(Nick ++ ": " ++ string:join(lists:filter(Filt, List), ", ")),
			 		 Conf));
		_ ->
		ok
	end;
process_message("summary" = Message, Rexp, To, State) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_SUMM ++ Rexp),
	io:format("You received: ~s: ~s ~s~n", [To, Message, Rexp]),
	Builds = proplists:get_keys(jsx:decode(erlang:list_to_binary(Body))),
	List = lists:map(fun(X) -> summ_gen(X, Rexp) end, Builds), 
	case string:tokens(To, "/")  of
		[Conf, Nick] ->
			exmpp_session:send_packet(State#state.session,
			 	exmpp_stanza:set_recipient(exmpp_message:groupchat(Nick ++ ": " ++ List),
			 		 Conf));
		_ ->
		ok
	end;
process_message(_Message, _Rexp, _To, _State) -> 
	ok.

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
    				case string:str(Msg, ?CONT) of
    					1 -> process_message(string:sub_string(Msg, 2), erlang:binary_to_list(From), State)
    				end;
    			[Msg, Rexp] ->
    				case string:str(Msg, ?CONT) of
    					1 -> process_message(string:sub_string(Msg, 2), Rexp, erlang:binary_to_list(From), State)
    				end
    		end;
    	0 ->
    		case string:tokens(Message, " ") of
			[Name, Msg] -> process_message(Msg, erlang:binary_to_list(From), State); 
			[Name, Msg, Rexp] -> process_message(Msg, Rexp, erlang:binary_to_list(From), State);
			_ -> ok
			end;
		_ -> ok
	end;

process_received_packet(_State, _Packet) ->
	ok.

summ_gen(Build, Rexp) ->
	{ok, {_, _, Body}} = httpc:request(?URL ++ ?API_SUMM ++ Rexp),
	ErlJson = jsx:decode(erlang:list_to_binary(Body)),
	Name = erlang:binary_to_list(proplists:get_value(<<"name">>, 
		lists:append(proplists:get_all_values(Build, ErlJson)))),
	BuildName = erlang:binary_to_list(proplists:get_value(<<"buildname">>,
		lists:append(proplists:get_all_values(Build, ErlJson)))),
	Tests = string:join([erlang:binary_to_list(X) || X <- proplists:get_all_values(<<"name">>, lists:append(proplists:get_value(<<"tests">>,
		lists:append(proplists:get_all_values(Build, ErlJson)))))], ", "),
	io_lib:format("~s on ~s, Failed tests: ~sn", [Name, BuildName, Tests]). 
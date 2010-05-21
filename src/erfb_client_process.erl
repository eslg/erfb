%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc RFB Client Process 
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_client_process).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(gen_fsm).

-export([start_link/0, prep_stop/2, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([wait_for_socket/2, wait_for_handshake/2, wait_for_security/2,
         wait_for_security_result/2, wait_for_server_init/2, running/2,
         server_failed/2]).
-export([send_event/2, set_pixel_format/2, set_encodings/2, update_request/3,
         client_disconnected/2, key/3, pointer/4, client_cut_text/2]).
-export([event_dispatcher/1]).

-include("erfblog.hrl").
%% @headerfile "erfb.hrl"
-include("erfb.hrl").

-record(state,  {socket             :: port(),
                 session            :: #session{},
                 event_dispatcher   :: pid(),
                 encodings = []     :: [{Code :: integer(), Module :: atom(), State :: term()}]}).

%% ====================================================================
%% External functions
%% ====================================================================
%% -- General ---------------------------------------------------------
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%% @hidden
-spec set_socket(fsmref(), port()) -> ok.
set_socket(Client, Socket) ->
    gen_fsm:send_event(Client, {socket_ready, Socket}).

%% @hidden
-spec prep_stop(fsmref(), term()) -> ok.
prep_stop(Client, Reason) ->
    client_disconnected(Client, Reason).

%% @spec event_dispatcher(fsmref()) -> pid()
%% @doc  Returns the event dispatcher associated to this process
-spec event_dispatcher(fsmref()) -> {ok, pid()}.
event_dispatcher(Client) ->
    gen_fsm:sync_send_all_state_event(Client, event_dispatcher).

%% -- Client -> Server messages ---------------------------------------
%% @spec send_event(fsmref(), client_event()) -> ok
%% @doc  Sends an client event to the server.  The event may be one of the following ones (each one represented with a corresponding record):
%%       <ul><li>client_connected</li><li>listener_disconnected</li><li>client_disconnected</li>
%%           <li>set_pixel_format</li><li>set_encodings</li><li>update_request</li><li>key</li>
%%           <li>pointer</li><li>client_cut_text</li></ul>
%%       For a description of the events, check the <a href="http://www.tigervnc.com/cgi-bin/rfbproto#client-to-server-messages">RFB Protocol Definition</a>
-spec send_event(fsmref(), client_event()) -> ok.
send_event(Client, Event) ->
    ?DEBUG("Sending ~p to server ~p~n", [element(1, Event), Client]),
    gen_fsm:send_event(Client, Event).

%% @spec set_pixel_format(Client::fsmref(), PixelFormat::#pixel_format{}) -> ok
%% @equiv send_event(Client, #set_pixel_format{pixel_format = PixelFormat})
-spec set_pixel_format(fsmref(), #pixel_format{}) -> ok.
set_pixel_format(Client, PixelFormat) ->
    send_event(Client, #set_pixel_format{pixel_format = PixelFormat}).

%% @spec set_encodings(fsmref(), [{integer(), atom()}]) -> ok
%% @equiv send_event(Client, #set_encodings{encodings = Encodings})
-spec set_encodings(fsmref(), [{integer(), atom()}]) -> ok.
set_encodings(Client, Encodings) ->
    send_event(Client, #set_encodings{encodings = Encodings}).

%% @spec update_request(fsmref(), boolean(), #box{}) -> ok
%% @equiv send_event(Client, #update_request{incremental = Incremental, box = Box})
-spec update_request(fsmref(), boolean(), #box{}) -> ok.
update_request(Client, Incremental, Box) ->
    send_event(Client, #update_request{incremental = Incremental,
                                       box         = Box}).

%% @spec key(fsmref(), boolean(), integer()) -> ok
%% @equiv send_event(Server, #key{down = Down, code = Code})
-spec key(fsmref(), boolean(), integer()) -> ok.
key(Server, Down, Code) ->
    send_event(Server, #key{down = Down, code = Code}).

%% @spec pointer(fsmref(), byte(), integer(), integer()) -> ok
%% @equiv send_event(Server, #pointer{button_mask = ButtonMask, x = X, y = Y})
-spec pointer(fsmref(), byte(), integer(), integer()) -> ok.
pointer(Server, ButtonMask, X, Y) ->
    send_event(Server, #pointer{button_mask = ButtonMask, x = X, y = Y}).

%% @spec client_cut_text(fsmref(), binary()) -> ok
%% @equiv send_event(Server, #client_cut_text{text = Text})
-spec client_cut_text(fsmref(), binary()) -> ok.
client_cut_text(Server, Text) ->
    send_event(Server, #client_cut_text{text = Text}).

%% @spec client_disconnected(fsmref(), Reason::term()) -> ok
%% @equiv send_event(Server, #client_disconnected{reason = Reason})
-spec client_disconnected(fsmref(), term()) -> ok.
client_disconnected(Server, Reason) ->
    send_event(Server, #client_disconnected{reason = Reason}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, wait_for_socket, #state{}, ?FSM_TIMEOUT}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, State} = erfb_encoding_raw:init(),
    {ok, wait_for_socket,
     #state{encodings = [{?ENCODING_RAW,
                          erfb_encoding_raw,
                          State}]}, ?FSM_TIMEOUT}.

%% ASYNC EVENTS -------------------------------------------------------
%% @hidden
-spec wait_for_socket(term(), #state{}) -> async_state_result().
wait_for_socket({socket_ready, Socket}, State) ->
    % Now we own the socket
    ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
    
    % We register the process
    {ok, {{IP1, IP2, IP3, IP4}, PeerPort}} = inet:peername(Socket),
    [PortStr | IPStrs] =
        lists:map(fun bstr:from_integer/1, [PeerPort, IP1, IP2, IP3, IP4]),
    ServerId = bstr:join([bstr:join(IPStrs, $.), PortStr, <<"server">>], $:),

    {next_state, wait_for_handshake, State#state{socket = Socket,
                                                 session= #session{server = ServerId}},
     ?FSM_TIMEOUT};
wait_for_socket(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_socket(Other, State) ->
    ?ERROR("Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, wait_for_socket, State, ?FSM_TIMEOUT}.

%% @hidden
-spec wait_for_handshake(term(), #state{}) -> async_state_result().
wait_for_handshake({data, <<?PROTOCOL_NAME, $\s, MajorStr:3/binary, $., MinorStr:3/binary, $\n>>},
                   State = #state{socket = S}) ->
    ServerVersion = {bstr:to_integer(MajorStr), bstr:to_integer(MinorStr)},
    Version =
        case ServerVersion of
            X when X < ?MIN_VERSION ->
                invalid;
            Y when Y >= ?MIN_VERSION andalso Y < ?MID_VERSION ->
                ?MIN_VERSION;
            ?MID_VERSION ->
                ?MID_VERSION;
            _ ->
                ?MAX_VERSION
        end,
    case Version of
        invalid ->
            ?ERROR("Invalid Server Version: ~p -> ~p~n", [<<MajorStr/binary, $., MinorStr/binary>>, ServerVersion]),
            {stop, {invalid_version, ServerVersion}, State};
        {Major, Minor} ->
            ?INFO("Version ~p handshaking...~n", [Version]),
            VersionStr = bstr:join([bstr:lpad(bstr:from_integer(Major), 3, $0),
                                    bstr:lpad(bstr:from_integer(Minor), 3, $0)],
                                    $.), 
            ok = erfb_utils:tcp_send(S,
                                     <<?PROTOCOL_NAME, $\s, VersionStr/binary, $\n>>,
                                     State),
            {next_state, wait_for_security,
             State#state{session = (State#state.session)#session{version = Version}},
             ?FSM_TIMEOUT}
    end;
wait_for_handshake(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_handshake(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%%NOTE: We allow only security type 1 - None because we're strictly using the
%%      Server -> Client model for our connections.
%% @hidden
-spec wait_for_security(term(), #state{}) -> async_state_result().
wait_for_security({data, <<?SECURITY_INVALID:4/unit:8, Rest/binary>>},
                  State = #state{session = #session{version = ?MIN_VERSION}}) ->
    ?TRACE("~n", []),
    {Error, _} = erfb_utils:get_full_string(Rest, State#state.socket),
    ?ERROR("Server security error for version 3.3: ~p~n", [Error]),
    {next_state, server_failed, State, ?FSM_TIMEOUT};
wait_for_security({data, <<?SECURITY_NONE:4/unit:8>>},
                  State = #state{session = #session{version = ?MIN_VERSION}}) ->
    ?DEBUG("Server security type for version 3.3: 1 - None~n", []),
    client_init(State);
wait_for_security({data, <<?SECURITY_VNC:4/unit:8>>},
                  State = #state{session = #session{version = ?MIN_VERSION}}) ->
    ?ERROR("Server security for version 3.3: 2 - VNC / not supported~n", []),
    {stop, security_not_supported, State};
wait_for_security({data, <<?SECURITY_INVALID, Rest/binary>>}, State) ->
    ?TRACE("~n", []),
    {Error, _} = erfb_utils:get_full_string(Rest, State#state.socket),
    ?ERROR("Server security error for version >= 3.7: ~p~n", [Error]),
    {next_state, server_failed, State, ?FSM_TIMEOUT};
wait_for_security({data, <<Count:1/binary, AllTypes/binary>>},
                  State = #state{session = #session{version = V},
                                 socket = S}) ->
    Types = lists:sublist(bstr:to_list(AllTypes), 1, hd(bstr:to_list(Count))),
    case lists:member(?SECURITY_NONE, Types) of
        true ->
            ok = erfb_utils:tcp_send(S, <<?SECURITY_NONE>>, State),
            case V of
                ?MID_VERSION ->
                    client_init(State);
                ?MAX_VERSION ->
                    {next_state, wait_for_security_result, State, ?FSM_TIMEOUT}
            end;
        false ->
            ?ERROR("Server doesn't accept any supported security type.~n\tServer Types: ~p~n", [Types]),
            {stop, security_not_supported, State}
    end;
wait_for_security(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_security(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec wait_for_security_result(term(), #state{}) -> async_state_result().
wait_for_security_result({data, <<?SECURITY_RESULT_OK:4/unit:8>>}, State) ->
    ?DEBUG("Security passed :)~n", []),
    client_init(State);
wait_for_security_result({data, <<?SECURITY_RESULT_NOK:4/unit:8, Rest/binary>>},
                         State = #state{session = #session{version = ?MAX_VERSION}}) ->
    {Error, _} = erfb_utils:get_full_string(Rest, State#state.socket),
    ?ERROR("Security not passed: ~p~n", [Error]),
    {next_state, server_failed, State, ?FSM_TIMEOUT};
wait_for_security_result({data, <<?SECURITY_RESULT_NOK:4/unit:8>>}, State) ->
    ?DEBUG("Security not passed :(~n", []),
    {next_state, server_failed, State, ?FSM_TIMEOUT};
wait_for_security_result(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_security_result(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec wait_for_server_init(term(), #state{}) -> async_state_result().
wait_for_server_init({data, <<BufferWidth:2/unit:8,
                              BufferHeight:2/unit:8,
                              PFBits:1/unit:8,
                              PFDepth:1/unit:8,
                              PFBigEndian:1/unit:8,
                              PFTrueColour:1/unit:8,
                              PFRedMax:2/unit:8,
                              PFGreenMax:2/unit:8,
                              PFBlueMax:2/unit:8,
                              PFRedShift:1/unit:8,
                              PFGreenShift:1/unit:8,
                              PFBlueShift:1/unit:8,
                              _PFPadding:3/unit:8,
                              Rest/binary>> = Data},
                     State = #state{session = Session}) ->
    {Name, NextMessage} =
        erfb_utils:get_full_string(Rest, State#state.socket),
    NewSession =
        Session#session{name           = Name,
                        width          = BufferWidth,
                        height         = BufferHeight,
                        pixel_format   = #pixel_format{bits_per_pixel = PFBits,
                                                       depth          = PFDepth,
                                                       big_endian     = PFBigEndian =/= ?FALSE,
                                                       true_colour    = PFTrueColour =/= ?FALSE,
                                                       red_max        = PFRedMax,
                                                       green_max      = PFGreenMax,
                                                       blue_max       = PFBlueMax,
                                                       red_shift      = PFRedShift,
                                                       green_shift    = PFGreenShift,
                                                       blue_shift     = PFBlueShift}},
    {ok, ED} = erfb_client_event_dispatcher:start_link_unregistered(),
    ok = erfb_client_event_dispatcher:notify(
           #server_connected{server             = NewSession#session.server,
                             client             = NewSession#session.client,
                             raw_data           = Data,
                             session            = NewSession,
                             event_dispatcher   = ED}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State#state{session           = NewSession,
                                              event_dispatcher  = ED}};
        _ ->
            running({data, NextMessage}, State#state{session    = NewSession,
                                              event_dispatcher  = ED})
    end;
wait_for_server_init(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_server_init(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec running({'data',<<_:8,_:_*8>>}, #state{}) -> {next_state, running, #state{}}.
running({data, <<?MSG_FRAMEBUFFER_UPDATE, _Padding:1/unit:8, FramebufferUpdate/binary>>},
        State) ->
    <<Length:2/unit:8, Rest/binary>> = FramebufferUpdate,
    {Rectangles, BytesRead, NextMessage, NewState} =
        read_rectangles(Length, Rest, State),
    ?TRACE("Original Bytes:~n\t~p~nBytesRead:~n\t~p~n", [FramebufferUpdate, <<?MSG_FRAMEBUFFER_UPDATE,
                                    _Padding:1/unit:8,
                                    Length:2/unit:8,
                                    BytesRead/binary>>]),
    ok = erfb_client_event_dispatcher:notify(
           State#state.event_dispatcher,
           #rfbupdate{server       = (NewState#state.session)#session.server,
                      client       = (NewState#state.session)#session.client,
                      raw_data     = <<?MSG_FRAMEBUFFER_UPDATE,
                                       _Padding:1/unit:8,
                                       Length:2/unit:8,
                                       BytesRead/binary>>,
                      rectangles   = Rectangles}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, NewState};
        _ ->
            running({data, NextMessage}, NewState)
    end;
running({data, <<?MSG_SET_COLOUR_MAP_ENTRIES, _Padding:1/unit:8, SetColourMapEntries/binary>>}, State) ->
    <<FirstColour:2/unit:8, Length:2/unit:8, Rest/binary>> =
        SetColourMapEntries,
    {ColoursStr, NextMessage} =
        case bstr:len(Rest) of
            L when L >= Length * 6 ->
                {bstr:substr(Rest, 1, Length * 6),
                 bstr:substr(Rest, Length * 6 + 1)};
            _ ->
                case erfb_utils:complete(Rest,
                                         Length * 6,
                                         State#state.socket) of
                    timeout ->
                        throw({stop, incomplete_colour_map_list, State});
                    Value ->
                        {Value, <<>>}
                end
        end,
    ColourMap =
        [ #colour{red   = Red,
                  green = Green,
                  blue  = Blue} || <<Red:2/unit:8,
                                     Green:2/unit:8,
                                     Blue:2/unit:8>> <= ColoursStr ],
    ok = erfb_client_event_dispatcher:notify(
            State#state.event_dispatcher,
            #set_colour_map_entries{server       = (State#state.session)#session.server,
                                    client       = (State#state.session)#session.client,
                                    raw_data     = <<?MSG_SET_COLOUR_MAP_ENTRIES,
                                                     _Padding:1/unit:8,
                                                     FirstColour:2/unit:8,
                                                     Length:2/unit:8,
                                                     ColoursStr/binary>>,
                                    first_colour = FirstColour,
                                    colours      = ColourMap}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_BELL, NextMessage/binary>>}, State) ->
    ok = erfb_client_event_dispatcher:notify(
           State#state.event_dispatcher,
           #bell{server     = (State#state.session)#session.server,
                 client     = (State#state.session)#session.client,
                 raw_data   = <<?MSG_BELL>>}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_SERVER_CUT_TEXT, _Padding:3/unit:8, ServerCutText/binary>>}, State) ->
    {Text, NextMessage} =
        erfb_utils:get_full_string(ServerCutText, State#state.socket),
    ok = erfb_client_event_dispatcher:notify(
           State#state.event_dispatcher,
           #server_cut_text{server      = (State#state.session)#session.server,
                            client      = (State#state.session)#session.client,
                            raw_data    = <<?MSG_SERVER_CUT_TEXT,
                                            _Padding:3/unit:8,
                                            (erfb_utils:build_string(Text))/binary>>,
                            text        = Text}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, Data = <<MessageType:1/unit:8, _/binary>>}, State) ->
    ok = erfb_client_event_dispatcher:notify(
           State#state.event_dispatcher,
           #unknown_message{server  = (State#state.session)#session.server,
                            client  = (State#state.session)#session.client,
                            type    = MessageType,
                            raw_data= Data}),
    {next_state, running, State};
running(#set_pixel_format{pixel_format  = PF,
                          raw_data      = RawData},
        State = #state{socket    = S,
                       session   = Session}) ->
    Message =
        case RawData of
            undefined ->
                PFBin = erfb_utils:pf_to_binary(PF),
                <<?MSG_SET_PIXEL_FORMAT:1/unit:8, 0:3/unit:8, PFBin/binary>>;
            RawData ->
                RawData
        end,
    ?DEBUG("Setting pixel format: ~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State#state{session =
                                         Session#session{pixel_format = PF}}};
running(#set_encodings{encodings = Encodings,
                       raw_data  = RawData},
        State = #state{socket    = S,
                       encodings = CurrentEncodings}) ->
    
    {Intersection, AddedEncodings} =
        lists:partition(
          fun({EC, _EM}) ->
                  lists:keymember(EC, 1, CurrentEncodings)
          end, Encodings),

    RemainingEncodings =
        lists:foldl(fun({erfb_encoding_raw, _, _} = E, Acc) ->
                            %%NOTE: Clients must accept raw encoding even
                            %%      if they don't specify it in the list
                            [E | Acc];
                       ({ECod, EMod, EState} = E, Acc) ->
                            case lists:keymember(ECod, 1, Intersection) of
                                true ->
                                    [E | Acc];
                                false ->
                                    ok = EMod:terminate(normal, EState),
                                    Acc
                            end
                    end, [], CurrentEncodings),
    
    FinalEncodings =
        lists:foldl(fun({ECod, EMod}, Acc) ->
                            {ok, EState} = EMod:init(),
                            [{ECod, EMod, EState} | Acc]
                    end, RemainingEncodings, AddedEncodings),
        
    Message =
        case RawData of
            undefined ->
                Length = length(Encodings),
                EncodingList =
                    << <<EncCod:4/signed-unit:8>> ||
                       {EncCod, _EncMod} <- Encodings >>,
                <<?MSG_SET_ENCODINGS:1/unit:8,
                  0:1/unit:8,
                  Length:2/unit:8,
                  EncodingList/binary>>;
            RawData ->
                RawData
        end,
    ?DEBUG("Setting encodings: ~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State#state{encodings = FinalEncodings}};
running(#update_request{incremental = Incremental,
                        box         = Box,
                        raw_data    = undefined},
        State = #state{socket    = S,
                       session   = #session{width = W,
                                            height= H}}) ->
    IncrementalByte = case Incremental of
                          true -> 1;
                          false -> 0
                      end,
    FinalWidth = case Box#box.width of
                     all -> W;
                     BW -> BW
                 end,
    FinalHeight = case Box#box.height of
                      all -> H;
                      BH -> BH
                  end,
    Message = <<?MSG_FRAMEBUFFER_UPDATE_REQUEST:1/unit:8,
                IncrementalByte:1/unit:8,
                (Box#box.x):2/unit:8,
                (Box#box.y):2/unit:8,
                FinalWidth:2/unit:8,
                FinalHeight:2/unit:8>>,
    ?DEBUG("Requesting update: ~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#update_request{raw_data = Message},
        State = #state{socket = S}) ->
    ?TRACE("Requesting update: ~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#key{down       = Down,
             code       = Code,
             raw_data   = undefined},
        State = #state{socket = S}) ->
    DownByte = case Down of
                   true -> 1;
                   false -> 0
               end,
    Message = <<?MSG_KEY_EVENT:1/unit:8,
                DownByte:1/unit:8,
                0:2/unit:8, %% Padding
                Code:4/unit:8>>,
    ?DEBUG("Key Event~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#key{raw_data = Message},
        State = #state{socket = S}) ->
    ?DEBUG("Key Event~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#pointer{button_mask = ButtonMask,
                 x = X, y = Y,
                 raw_data = undefined},
        State = #state{socket = S}) ->
    Message = <<?MSG_POINTER_EVENT:1/unit:8,
                ButtonMask:1/unit:8,
                X:2/unit:8,
                Y:2/unit:8>>,
    ?DEBUG("Pointer Event~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#pointer{raw_data = Message},
        State = #state{socket = S}) ->
    ?DEBUG("Pointer Event~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#client_cut_text{text       = Text,
                         raw_data   = undefined},
        State = #state{socket = S}) ->
    TextStr = erfb_utils:build_string(Text),
    Message = <<?MSG_CLIENT_CUT_TEXT:1/unit:8,
                0:3/unit:8, %% Padding
                TextStr/binary>>,
    ?DEBUG("Cutting the Text~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#client_cut_text{raw_data = Message},
        State = #state{socket = S}) ->
    ?DEBUG("Cutting the Text~n", []), ?TRACE("\t~p~n", [Message]),
    ok = erfb_utils:tcp_send(S, Message, State),
    {next_state, running, State};
running(#client_disconnected{reason = Reason}, State) ->
    ?INFO("Client disconnected: ~p~n", [Reason]),
    {stop, normal, State}.

%% @hidden
-spec server_failed(term(), #state{}) -> async_state_result().
server_failed(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, server_failed, State};
server_failed(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% OTHER EVENTS -------------------------------------------------------
%% @hidden
-spec handle_event(term(), atom(), #state{}) -> async_state_result().
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% @hidden
-spec handle_sync_event(term(), term(), atom(), #state{}) -> sync_state_result().
handle_sync_event(event_dispatcher, _From, StateName,
                  StateData = #state{event_dispatcher = ED}) ->
    {reply, {ok, ED}, StateName, StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% @hidden
-spec handle_info(term(), atom(), #state{}) -> async_state_result().
handle_info({tcp, Socket, Bin}, StateName, #state{socket = Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    ok = inet:setopts(Socket, [{active, false}]),
    Result = ?MODULE:StateName({data, Bin}, StateData),
    ok = inet:setopts(Socket, [{active, once}]),
    Result;

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket = Socket, session = #session{server = ServerId}} = StateData) ->
    ?INFO("Server ~s disconnected.\n", [ServerId]),
    {stop, normal, StateData};

handle_info({'EXIT', ED, Reason}, StateName,
            State = #state{event_dispatcher = ED}) ->
    ?WARN("Linked event dispatcher [~p] terminated with reason: ~p~n", [ED, Reason]),
    {ok, NewED} = erfb_client_event_dispatcher:start_link_unregistered(),
    {next_state, StateName, State#state{event_dispatcher = NewED}};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% @hidden
-spec terminate(term(), atom(), #state{}) -> ok.
terminate(Reason, _StateName, #state{socket     = Socket,
                                     session    = #session{server = ServerId,
                                                           client = ClientId},
                                     encodings  = Encodings}) ->
    (catch gen_tcp:close(Socket)),
    lists:foreach(
      fun({_, EMod, EState}) ->
              (catch EMod:terminate(Reason, EState))
      end, Encodings),
    erfb_client_event_dispatcher:notify(
           #server_disconnected{server = ServerId,
                                client = ClientId,
                                reason = Reason}).

%% @hidden
-spec code_change(term(), atom(), #state{}, any()) -> {ok, atom(), #state{}}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec client_init(#state{}) -> {'next_state','wait_for_server_init',#state{}, ?FSM_TIMEOUT}.
client_init(State = #state{socket = S}) ->
    ok = erfb_utils:tcp_send(S, <<1>>, State),
    ?DEBUG("Client Init sent~n", []),
    {next_state, wait_for_server_init, State, ?FSM_TIMEOUT}.

-spec read_rectangles(pos_integer(), binary(), #state{}) -> {Result :: [#rectangle{}], Read :: binary(), Rest :: binary(), NewState :: #state{}}.
read_rectangles(Count, Stream, State) ->
    try
        ?DEBUG("About to read ~p rects...~n~n", [Count]),
        read_rectangles(Count, Stream, State, <<>>, [])
    catch
        _:Error ->
            ?ERROR("Error:~n~p reading rectangles~n", [Error]),
            throw(Error)
    end.

-spec read_rectangles(integer(), binary(), #state{}, binary(), [#rectangle{}]) -> {Result :: [#rectangle{}], Read :: binary(), Rest :: binary(), NewState :: #state{}}.
read_rectangles(0, Rest, State, BytesRead, Rectangles) ->
    {lists:reverse(Rectangles), BytesRead, Rest, State};
read_rectangles(Missing, <<Head:12/binary, Rest/binary>>,
                State = #state{session = #session{width = TotW,
                                                  height= TotH}},
                BytesRead, Accum) ->
    <<X:2/unit:8,
      Y:2/unit:8,
      W:2/unit:8,
      H:2/unit:8,
      EncodingCode:4/signed-unit:8>> = Head,
    Box = #box{x = X, y = Y, width = W, height = H},
    ?DEBUG("Reading the next ~p rects~n", [Missing]),
    if
        X > TotW;
        Y > TotH;
        X+W > TotW;
        Y+H > TotH ->
            %%NOTE: Just a warning, because some encodings use this parameters
            %%      to inform something else than the "real" box
            ?WARN("Box out of bounds when ~p rects missing:~n\t~p~n",
                  [Missing, Box]);
        true ->
            void
    end,
    
    {Rectangle, Bytes, Next, NextState} =
        read_rectangle(Box, EncodingCode, Rest, State),
    read_rectangles(Missing - 1, Next, NextState,
                    <<BytesRead/binary, Head/binary, Bytes/binary>>,
                    [Rectangle | Accum]);
read_rectangles(Missing, Rest, State, BytesRead, Accum) ->
    ?DEBUG("We have just ~p to read but we need ~p rects more.~n", [Rest, Missing]),
    case erfb_utils:complete(Rest,
                             12, %%NOTE: Rect box length - i.e. we're looking for the next rectangle
                             State#state.socket) of
        timeout ->
            throw({stop, not_enough_data, State});
        ShortRest when erlang:size(ShortRest) < 12 -> %%NOTE: Cannot read another rectangle
            throw({stop, not_enough_data, State});
        NewRest ->
            read_rectangles(Missing, NewRest, State, BytesRead, Accum)
    end.

-spec read_rectangle(#box{}, integer(), binary(), #state{}) -> {#rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read_rectangle(Box, ECode, Stream,
               State = #state{socket     = S,
                              encodings  = Encodings,
                              session    = #session{pixel_format = PF}}) ->
    {ECode, EMod, EState} =
        case lists:keyfind(ECode, 1, Encodings) of
            false ->
                ?ERROR("Unknown Encoding ~p for box ~p.~n\tNot found in: ~p~n", [ECode, Box, 
                                                                                 Encodings]),
                throw({stop, {unknown_encoding, ECode}, State});
            Enc ->
                Enc
        end,
    try
        ?TRACE("Reading rect with encoding ~p: ~p~n", [ECode, EMod]),
        {ok, Data, Read, Rest, NewEState} =
            EMod:read(PF, Box, Stream, S, EState),
        NewEncodings =
            lists:keystore(ECode, 1, Encodings, {ECode, EMod, NewEState}),
        {#rectangle{box         = Box,
                    encoding    = ECode,
                    data        = Data}, Read, Rest, State#state{encodings = NewEncodings}}
    catch
        _:Error ->
            ?ERROR("Error reading ~p with ~p encoding:~n\t~p~n", [Box, EMod, Error]),
            ?TRACE("Original bytes:~n~p~n", [Stream]),
            throw({stop, Error, State})
    end.
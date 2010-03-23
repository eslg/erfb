%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc RFB Server Process 
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_server_process).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(gen_fsm).

-export([start_link/2, set_socket/2, prep_stop/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([wait_for_socket/2, wait_for_handshake/2, wait_for_security_type/2,
         wait_for_vnc_response/2, wait_for_client_init/2, running/2, running/3]).
-export([send_event/2]).
-export([update/2, set_colour_map_entries/3, bell/1, server_cut_text/2, server_disconnected/2]).
-export([event_dispatcher/1]).

-include("erfblog.hrl").
%% @headerfile "erfb.hrl"
-include("erfb.hrl").

-record(state,  {socket             :: port(),
                 session            :: #session{},
                 vnc_challenge      :: binary(),
                 event_dispatcher   :: pid(),
                 encodings = []     :: [{Code :: integer(), Module :: atom(), State :: term()}]}).

%% ====================================================================
%% External functions
%% ====================================================================
%% -- General ---------------------------------------------------------
%% @hidden
-spec start_link(#session{}, [{integer(), atom()}]) -> {ok, pid()}.
start_link(Server, Encodings) ->
    gen_fsm:start_link(?MODULE, {Server, Encodings}, []).

%% @hidden
-spec set_socket(pid(), port()) -> ok.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%% @hidden
-spec prep_stop(fsmref(), term()) -> ok.
prep_stop(Server, Reason) ->
    server_disconnected(Server, Reason).

%% @spec event_dispatcher(fsmref()) -> pid()
%% @doc  Returns the event dispatcher associated to this process
-spec event_dispatcher(fsmref()) -> {ok, pid()}.
event_dispatcher(Server) ->
    gen_fsm:sync_send_all_state_event(Server, event_dispatcher).

%% -- Server -> Client messages ---------------------------------------
%% @spec send_event(fsmref(), server_event()) -> ok
%% @doc  Sends an server event to the client.  The event may be one of the following ones (each one represented with a corresponding record):
%%       <ul><li>server_connected</li><li>server_disconnected</li><li>set_colour_map_entries</li>
%%           <li>update</li><li>bell</li><li>server_cut_text</li></ul>
%%       For a description of the events, check the <a href="http://www.tigervnc.com/cgi-bin/rfbproto#client-to-server-messages">RFB Protocol Definition</a>
-spec send_event(fsmref(), server_event()) -> ok.
send_event(Server, Event) ->
    gen_fsm:sync_send_event(Server, Event).

%% @spec update(fsmref(), [#rectangle{}]) -> ok
%% @equiv send_event(Server, #update{rectangles = Rectangles})
-spec update(fsmref(), [#rectangle{}]) -> ok.
update(Server, Rectangles) ->
    send_event(Server, #update{rectangles = Rectangles}).

%% @spec set_colour_map_entries(fsmref(), integer(), [#colour{}]) -> ok
%% @equiv send_event(Server, #set_colour_map_entries{first_colour = FirstColour, colours = Colours})
-spec set_colour_map_entries(fsmref(), integer(), [#colour{}]) -> ok.
set_colour_map_entries(Server, FirstColour, Colours) ->
    send_event(Server, #set_colour_map_entries{first_colour = FirstColour,
                                               colours = Colours}).

%% @spec bell(fsmref()) -> ok
%% @equiv send_event(Server, #bell{})
-spec bell(fsmref()) -> ok.
bell(Server) ->
    send_event(Server, #bell{}).

%% @spec server_cut_text(fsmref(), binary()) -> ok
%% @equiv send_event(Server, #server_cut_text{text = Text})
-spec server_cut_text(fsmref(), binary()) -> ok.
server_cut_text(Server, Text) ->
    send_event(Server, #server_cut_text{text = Text}).

%% @spec server_disconnected(fsmref(), term()) -> ok
%% @equiv send_event(Server, #server_disconnected{reason = Reason})
-spec server_disconnected(fsmref(), term()) -> ok.
server_disconnected(Server, Reason) ->
    send_event(Server, #server_disconnected{reason = Reason}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init({undefined | #session{}, [{integer(), atom()}]}) -> {ok, wait_for_socket, #state{}, ?FSM_TIMEOUT}.
init({Session, EncodingMods}) ->
    process_flag(trap_exit, true),
    _Seed = random:seed(erlang:now()),
    PreEncodings = [{ECod, EMod, not_initialized} || {ECod, EMod} <- EncodingMods],
    {ok, State} = erfb_encoding_raw:init(),
    Encodings = lists:keystore(?ENCODING_RAW, 1, PreEncodings,
                               {?ENCODING_RAW,
                                erfb_encoding_raw,
                                State}),
    {ok, wait_for_socket, #state{session    = Session,
                                 encodings  = Encodings}, ?FSM_TIMEOUT}.


%% ASYNC EVENTS -------------------------------------------------------
%% @hidden
-spec wait_for_socket(term(), #state{}) -> async_state_result().
wait_for_socket({socket_ready, Socket}, State = #state{session = Session}) ->
    % Now we own the socket
    ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
    
    % We register the process
    {ok, {{IP1, IP2, IP3, IP4}, PeerPort}} = inet:peername(Socket),
    [PortStr | IPStrs] =
        lists:map(fun bstr:from_integer/1, [PeerPort, IP1, IP2, IP3, IP4]),
    
    ClientId = bstr:join([bstr:join(IPStrs, $.), PortStr, <<"client">>], $:),
    
    {Major, Minor} = Session#session.version,
    VersionStr = bstr:join([bstr:lpad(bstr:from_integer(Major), 3, $0),
                            bstr:lpad(bstr:from_integer(Minor), 3, $0)],
                           $.), 
    ok = gen_tcp:send(Socket, <<?PROTOCOL_NAME, $\s, VersionStr/binary, $\n>>),
    {next_state, wait_for_handshake,
     State#state{socket = Socket,
                 session= (State#state.session)#session{client = ClientId}},
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
wait_for_handshake({data, Data = <<?PROTOCOL_NAME, $\s, MajorStr:3/binary, $., MinorStr:3/binary, $\n>>},
                   State = #state{socket = S,
                                  session = #session{version  = SV,
                                                     security = Security}}) ->
    ?DEBUG("Handshake: ~p~n", [Data]),
    ClientVersion = {bstr:to_integer(MajorStr), bstr:to_integer(MinorStr)},
    Version =
        case ClientVersion of
            X when X < ?MIN_VERSION ->
                invalid;
            X when X > SV->
                invalid; %%NOTE: The client cannot choose a version we can't understand
            Y when Y >= ?MIN_VERSION andalso Y < ?MID_VERSION ->
                ?MIN_VERSION;
            CV ->
                CV
        end,
    case Version of
        invalid ->
            ?ERROR("Invalid Client Version: ~p -> ~p~n", [<<MajorStr/binary, $., MinorStr/binary>>, ClientVersion]),
            Error = erfb_utils:build_string(<<"Unsupported Version">>),
            case SV of
                ?MIN_VERSION ->
                    ok = gen_tcp:send(S, <<?SECURITY_INVALID:4/unit:8, Error/binary>>);
                _ ->
                    ok = gen_tcp:send(S, <<?SECURITY_INVALID:1/unit:8, Error/binary>>)
            end,    
            {stop, normal, State};
        ?MIN_VERSION ->
            ?INFO("Version ~p handshaking...~n", [Version]),
            NextState = State#state{session =
                                        (State#state.session)#session{version = Version}},
            case Security of
                [{vnc, _Password}] ->
                    ok = gen_tcp:send(S, <<?SECURITY_VNC:4/integer-unit:8>>),
                    vnc_security(NextState);
                _ ->
                    ok = gen_tcp:send(S, <<?SECURITY_NONE:4/integer-unit:8>>),
                    {next_state, wait_for_client_init, NextState, ?FSM_TIMEOUT}
            end;
        _ ->
            ?INFO("Version ~p handshaking (~p)...~n", [Version, Security]),
            NextState = State#state{session =
                                        (State#state.session)#session{version = Version}},
            SecurityTypes = erlang:length(Security),
            SecurityCodes = <<<<(case Sec of
                                      none -> ?SECURITY_NONE;
                                      {vnc, _} -> ?SECURITY_VNC
                                 end)>> || Sec <- Security>>,
            ok = gen_tcp:send(S, <<SecurityTypes:1/unit:8, SecurityCodes/binary>>),
            {next_state, wait_for_security_type, NextState, ?FSM_TIMEOUT}
    end;
wait_for_handshake(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_handshake(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec wait_for_security_type(term(), #state{}) -> async_state_result().
wait_for_security_type({data, <<?SECURITY_NONE>>}, State = #state{session = #session{version = ?MID_VERSION}}) ->
    ?DEBUG("Security None choosen~n", []),
    {next_state, wait_for_client_init, State, ?FSM_TIMEOUT};
wait_for_security_type({data, <<?SECURITY_NONE>>}, State = #state{socket = S,
                                                                  session = #session{version = V}})
  when V >= ?MAX_VERSION ->
    ?DEBUG("Security None choosen~n", []),
    ok = gen_tcp:send(S, <<?SECURITY_RESULT_OK:4/unit:8>>),
    {next_state, wait_for_client_init, State, ?FSM_TIMEOUT};
wait_for_security_type({data, <<?SECURITY_VNC>>}, State) ->
    ?DEBUG("VNC Security choosen~n", []),
    vnc_security(State);
wait_for_security_type(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_security_type(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec wait_for_vnc_response(term(), #state{}) -> async_state_result().
wait_for_vnc_response({data, Response},
                      State = #state{vnc_challenge  = Challenge,
                                     session        = #session{security = Security,
                                                               version  = Version},
                                     socket         = S}) ->
    [Password | _] = [P || {vnc, P} <- Security],
    %%NOTE: We ignore the bytes after the 8 because, at least cotvnc does that too
    %%      And we also flip each byte, as cotvnc does
    DesKey = << <<(flip_byte(B))/binary>> ||
        <<B:1/binary>> <= bstr:rpad(bstr:substr(Password, 1, 8), 8, 0) >>,
    Cipher =
        << <<(crypto:des_cbc_encrypt(DesKey,
                                     <<0:8/unit:8>>,
                                     Block))/binary>> ||
           <<Block:8/binary>> <= Challenge >>,
    ?DEBUG("VNC response:~n\tChallenge= ~p, Password= ~p,~nResponse= ~p~n", [Challenge, Password, Response]),
    case Cipher of
        Response ->
            ?DEBUG("Password OK~n", []),
            ok = gen_tcp:send(S, <<?SECURITY_RESULT_OK:4/unit:8>>),
            %% ok = gen_tcp:send(S, <<?SECURITY_RESULT_OK:4/unit:8>>),
            {next_state, wait_for_client_init, State, ?FSM_TIMEOUT};
        _ ->
            ?ERROR("Invalid Password~nVNC response:~n\tChallenge= ~p,~n\tPassword= ~p,~n\tResponse= ~p~n\tCipher= ~p~n", [Challenge, DesKey, Response, Cipher]),
            ok = gen_tcp:send(S, <<?SECURITY_RESULT_NOK:4/unit:8>>),
            case Version of
                V when V >= ?MAX_VERSION ->
                    ok = gen_tcp:send(S, erfb_utils:build_string(<<"Invalid Password">>));
                _ ->
                    void
            end,
            {stop, normal, State}
    end;
wait_for_vnc_response(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_vnc_response(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec wait_for_client_init(term(), #state{}) -> async_state_result().
wait_for_client_init({data, <<?FALSE>>}, State = #state{socket = S}) -> %%NOTE: exclusive access request
    ?ERROR("Client wants exclusive access~n", []),
    ok = gen_tcp:send(S, erfb_utils:build_string(<<"Exclusive access is forbidden">>)),
    {stop, normal, State};
wait_for_client_init({data, <<Flag:1/unit:8>>},
                     State = #state{socket = S,
                                    session= Session}) ->
    ?DEBUG("Shared flag ~p received~n", [Flag]),
    Width   = Session#session.width,
    Height  = Session#session.height,
    PFBin   = erfb_utils:pf_to_binary(Session#session.pixel_format),
    NameBin = erfb_utils:build_string(Session#session.name),
    Message = <<Width:2/unit:8, Height:2/unit:8, PFBin/binary, NameBin/binary>>,
    ?DEBUG("Initializing~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {ok, ED} = erfb_server_event_dispatcher:start_link_unregistered(),
    ok = erfb_server_event_dispatcher:notify(
           #client_connected{server             = Session#session.server,
                             client             = Session#session.client,
                             raw_data           = Message,
                             session            = Session,
                             event_dispatcher   = ED}),
    {next_state, running, State#state{event_dispatcher = ED}, ?FSM_TIMEOUT};
wait_for_client_init(timeout, State) ->
    ?ERROR("Timeout~n", []),
    {stop, timeout, State};
wait_for_client_init(Event, State) ->
    ?ERROR("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec running({data,<<_:8,_:_*8>>},#state{}) -> {next_state, running, #state{}}.
running({data, <<?MSG_SET_PIXEL_FORMAT, _Padding:3/unit:8, SetPixelFormat/binary>>}, State) ->
    <<PFData:16/binary,
      NextMessage/binary>> = SetPixelFormat,
    <<PFBits:1/unit:8,
      PFDepth:1/unit:8,
      PFBigEndian:1/unit:8,
      PFTrueColour:1/unit:8,
      PFRedMax:2/unit:8,
      PFGreenMax:2/unit:8,
      PFBlueMax:2/unit:8,
      PFRedShift:1/unit:8,
      PFGreenShift:1/unit:8,
      PFBlueShift:1/unit:8,
      _PFPadding:3/unit:8>> = PFData,
    NewPixelFormat = #pixel_format{bits_per_pixel = PFBits,
                                   depth          = PFDepth,
                                   big_endian     = PFBigEndian =/= ?FALSE,
                                   true_colour    = PFTrueColour =/= ?FALSE,
                                   red_max        = PFRedMax,
                                   green_max      = PFGreenMax,
                                   blue_max       = PFBlueMax,
                                   red_shift      = PFRedShift,
                                   green_shift    = PFGreenShift,
                                   blue_shift     = PFBlueShift},
    Session = State#state.session,
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #set_pixel_format{server         = Session#session.server,
                             client         = Session#session.client,
                             raw_data       = <<?MSG_SET_PIXEL_FORMAT,
                                                _Padding:3/unit:8,
                                                PFData/binary>>,
                             pixel_format   = NewPixelFormat}),
    NewState = State#state{session =
                               Session#session{pixel_format = NewPixelFormat}},
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, NewState};
        _ ->
            running({data, NextMessage}, NewState)
    end;
running({data, <<?MSG_SET_ENCODINGS, _Padding:1/unit:8, SetEncodings/binary>>},
        State = #state{encodings = CurrentEncodings}) ->
    <<Length:2/unit:8, Rest/binary>> = SetEncodings,
    {EncodingsStr, NextMessage} =
        case bstr:len(Rest) of
            L when L >= Length * 4  ->
                {bstr:substr(Rest, 1, Length * 4),
                 bstr:substr(Rest, Length * 4 + 1)};
            _ ->
                case erfb_utils:complete(Rest,
                                         Length * 4,
                                         State#state.socket) of
                    timeout ->
                        throw({stop, incomplete_encoding_list, State});
                    Value ->
                        {Value, <<>>}
                end
        end,
    Encodings =
        [lists:keyfind(Code, 1, CurrentEncodings) ||
           <<Code:4/signed-unit:8>> <= EncodingsStr,
           false =/= lists:keyfind(Code, 1, CurrentEncodings)],
    Unrecognized =
        [Code || <<Code:4/signed-unit:8>> <= EncodingsStr,
           false =:= lists:keyfind(Code, 1, CurrentEncodings)],
    ?INFO("Unrecognized encodings: ~p~n", [Unrecognized]),

    FinalEncodings =
        lists:foldl(fun({erfb_encoding_raw, _, _} = E, Acc) ->
                            %%NOTE: Servers must accept raw encoding even
                            %%      if they don't specify it in the list
                            %%      and it was already initialized
                            [E | Acc];
                       ({ECode, EMod, not_initialized} = E, Acc) ->
                            case lists:keymember(ECode, 1, Encodings) of
                                true ->
                                    {ok, EState} = EMod:init(),
                                    [{ECode, EMod, EState} | Acc];
                                false ->
                                    [E | Acc]
                            end;
                       ({ECode, EMod, EState} = E, Acc) ->
                            case lists:keymember(ECode, 1, Encodings) of
                                true ->
                                    [E | Acc];
                                false ->
                                    ok = EMod:terminate(normal, EState),
                                    [{ECode, EMod, not_initialized} | Acc]
                            end
                    end, [], CurrentEncodings),

    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #set_encodings{server     = (State#state.session)#session.server,
                          client     = (State#state.session)#session.client,
                          raw_data   = <<?MSG_SET_ENCODINGS,
                                         _Padding:1/unit:8,
                                         Length:2/unit:8,
                                         EncodingsStr/binary>>,
                          encodings  = [{ECode, EMod} || {ECode, EMod, _} <- Encodings]}),

    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State#state{encodings = FinalEncodings}};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_FRAMEBUFFER_UPDATE_REQUEST, FramebufferUpdateRequest/binary>>}, State) ->
    <<Data:9/binary, NextMessage/binary>> = FramebufferUpdateRequest,
    <<IncrementalByte:1/unit:8,
      X:2/unit:8, Y:2/unit:8, W:2/unit:8, H:2/unit:8>> = Data,
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #update_request{server     = (State#state.session)#session.server,
                           client     = (State#state.session)#session.client,
                           raw_data   = <<?MSG_FRAMEBUFFER_UPDATE_REQUEST, Data/binary>>,
                           incremental= IncrementalByte =/= ?FALSE,
                           box        = #box{x = X, y = Y,
                                             width = W, height = H}}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_KEY_EVENT, KeyEvent/binary>>}, State) ->
    <<Data:7/binary, NextMessage/binary>> = KeyEvent, 
    <<DownByte:1/unit:8,
      _Padding:2/unit:8,
      Code:4/unit:8>> = Data,
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #key{server      = (State#state.session)#session.server,
                client      = (State#state.session)#session.client,
                raw_data    = <<?MSG_KEY_EVENT, Data/binary>>,
                down        = DownByte =/= ?FALSE,
                code        = Code}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_POINTER_EVENT, PointerEvent/binary>>}, State) ->
    <<Data:5/binary, NextMessage/binary>> = PointerEvent,
    <<ButtonMask:1/unit:8, X:2/unit:8, Y:2/unit:8>> = Data,
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #pointer{server      = (State#state.session)#session.server,
                    client      = (State#state.session)#session.client,
                    raw_data    = <<?MSG_POINTER_EVENT, Data/binary>>,
                    button_mask = ButtonMask,
                    x           = X,
                    y           = Y}),
    case bstr:len(NextMessage) of
        0 ->
            {next_state, running, State};
        _ ->
            running({data, NextMessage}, State)
    end;
running({data, <<?MSG_CLIENT_CUT_TEXT, _Padding:3/unit:8, ClientCutText/binary>>}, State) ->
    {Text, NextMessage} = erfb_utils:get_full_string(ClientCutText,
                                                     State#state.socket),
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #client_cut_text{server      = (State#state.session)#session.server,
                            client      = (State#state.session)#session.client,
                            raw_data    = <<?MSG_CLIENT_CUT_TEXT,
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
    ok = erfb_server_event_dispatcher:notify(
           State#state.event_dispatcher,
           #unknown_message{server   = (State#state.session)#session.server,
                            client   = (State#state.session)#session.client,
                            type     = MessageType,
                            raw_data = Data}),
    {next_state, running, State}.

%% @hidden
-spec running(term(), term(), #state{}) -> sync_state_result().
running(#update{rectangles  = Rs,
                raw_data    = undefined}, 
        From, State = #state{socket = S}) ->
    Length      = erlang:length(Rs),
    {Rectangles, NewState} =
        lists:foldl(
          fun(R, {AccRsStr, AccState}) ->
                  {NewR, NextState} =
                      write_rectangle(R, AccState),
                  {<<AccRsStr/binary, NewR/binary>>, NextState}
          end, {<<>>, State}, Rs),
    Message     = <<?MSG_FRAMEBUFFER_UPDATE:1/unit:8,
                    0:1/unit:8, %% Padding
                    Length:2/unit:8,
                    Rectangles/binary>>,
    gen_fsm:reply(From, ok),
    ?DEBUG("Updating~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, NewState};
running(#update{raw_data    = Message},
        From, State = #state{socket = S}) ->
    gen_fsm:reply(From, ok),
    ?DEBUG("Updating~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};
running(#set_colour_map_entries{first_colour = First,
                                colours      = Colours,
                                raw_data     = undefined},
        From, State = #state{socket = S}) ->
    Length      = erlang:length(Colours),
    ColoursStr  = << <<(C#colour.red):2/unit:8,
                       (C#colour.green):2/unit:8,
                       (C#colour.blue):2/unit:8>> || C <- Colours >>,
    Message     = <<?MSG_SET_COLOUR_MAP_ENTRIES:1/unit:8,
                    0:1/unit:8, %% Padding
                    First:2/unit:8,
                    Length:2/unit:8,
                    ColoursStr/binary>>,
    gen_fsm:reply(From, ok),
    ?DEBUG("Setting colour entries~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};    
running(#set_colour_map_entries{raw_data    = Message},
        From, State = #state{socket = S}) ->
    gen_fsm:reply(From, ok),
    ?DEBUG("Setting colour entries~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};
running(#bell{raw_data = undefined},
        From, State = #state{socket = S}) ->
    Message = <<?MSG_BELL>>,
    gen_fsm:reply(From, ok),
    ?DEBUG("Ringing the Bell~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};    
running(#bell{raw_data = Message},
        From, State = #state{socket = S}) ->
    gen_fsm:reply(From, ok),
    ?DEBUG("Ringing the Bell~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};    
running(#server_cut_text{text       = Text,
                         raw_data   = undefined},
        From, State = #state{socket = S}) ->
    TextStr = erfb_utils:build_string(Text),
    Message = <<?MSG_SERVER_CUT_TEXT:1/unit:8,
                0:3/unit:8, %% Padding
                TextStr/binary>>,
    gen_fsm:reply(From, ok),
    ?DEBUG("Cutting the Text~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};
running(#server_cut_text{raw_data = Message},
        From, State = #state{socket = S}) ->
    gen_fsm:reply(From, ok),
    ?DEBUG("Cutting the Text~n", []), ?TRACE("\t~p~n", [Message]),
    ok = gen_tcp:send(S, Message),
    {next_state, running, State};
running(#server_disconnected{reason = Reason}, _From, State) ->
    ?INFO("Server disconnected: ~p~n", [Reason]),
    {stop, normal, ok, State}.


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
            #state{socket = Socket, session = #session{client = Client}} = StateData) ->
    ?INFO("Client ~s disconnected.\n", [Client]),
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
      fun({_, _, not_initialized}) ->
              void;
         ({_, EMod, EState}) ->
              (catch EMod:terminate(Reason, EState))
      end, Encodings),
    erfb_server_event_dispatcher:notify(
      #client_disconnected{server = ServerId,
                           client = ClientId,
                           reason = Reason}).

%% @hidden
-spec code_change(term(), atom(), #state{}, any()) -> {ok, atom(), #state{}}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec vnc_security(#state{}) -> {next_state,wait_for_vnc_response,#state{},?FSM_TIMEOUT}.
vnc_security(State = #state{socket = S}) ->
    Challenge = << <<(random:uniform(255)):1/unit:8>> || _ <- lists:seq(1,16) >>,
    ok = gen_tcp:send(S, Challenge),
    {next_state, wait_for_vnc_response,
     State#state{vnc_challenge = Challenge}, ?FSM_TIMEOUT}.

-spec write_rectangle(#rectangle{}, #state{}) -> {binary(), #state{}}.
write_rectangle(#rectangle{box      = Box,
                           encoding = ECode,
                           data     = Data},
                State = #state{encodings= Encodings,
                               session  = Session = #session{width = W,
                                                             height= H}}) ->
    {ECode, EMod, EState} =
        case lists:keyfind(ECode, 1, Encodings) of
            false ->
                ?ERROR("Unknown Encoding ~p.  Not found in: ~p~n", [ECode, Encodings]),
                throw({stop, {unknown_encoding, ECode}, State});
            {EC, EM, not_initialized} ->
                {ok, FirstEState} = EM:init(),
                {EC, EM, FirstEState};
            Enc ->
                Enc
        end,
    FinalWidth = case Box#box.width of
                     all -> W;
                     BW -> BW
                 end,
    FinalHeight = case Box#box.height of
                      all -> H;
                      BH -> BH
                  end,
    try EMod:write(Session, Box, Data, EState) of
        {ok, EncodedData, NewEState} -> 
            NewEncodings =
                lists:keystore(ECode, 1, Encodings, {ECode, EMod, NewEState}),
            {<<(Box#box.x):2/unit:8,
               (Box#box.y):2/unit:8,
               FinalWidth:2/unit:8,
               FinalHeight:2/unit:8,
               ECode:4/signed-unit:8,
               EncodedData/binary>>, State#state{encodings = NewEncodings}};
        {ok, EncodedData, NewEState, NewSession} ->
            NewEncodings =
                lists:keystore(ECode, 1, Encodings, {ECode, EMod, NewEState}),
            {<<(Box#box.x):2/unit:8,
               (Box#box.y):2/unit:8,
               FinalWidth:2/unit:8,
               FinalHeight:2/unit:8,
               ECode:4/signed-unit:8,
               EncodedData/binary>>,
             State#state{encodings  = NewEncodings,
                         session    = NewSession}};
        {error, invalid_data, NewEState} ->
            ?ERROR("Error writing ~p with ~p encoding:~n\t~p~n", [Box, EMod, invalid_data]),
            ?TRACE("Original data:~n~p~n", [Data]),
            NewEncodings =
                lists:keystore(ECode, 1, Encodings, {ECode, EMod, NewEState}),
            throw({stop, {error, invalid_data}, State#state{encodings = NewEncodings}})
    catch
        _:Error ->
            ?ERROR("Error writing ~p with ~p encoding:~n\t~p~n", [Box, EMod, Error]),
            ?TRACE("Original data:~n~p~n", [Data]),
            throw({stop, Error, State})
    end.

-spec flip_byte(<<_:8>>) -> <<_:8>>.
flip_byte(<<B1:1/unit:1, B2:1/unit:1, B3:1/unit:1, B4:1/unit:1,
            B5:1/unit:1, B6:1/unit:1, B7:1/unit:1, B8:1/unit:1>>) ->
    <<B8:1/unit:1, B7:1/unit:1, B6:1/unit:1, B5:1/unit:1,
      B4:1/unit:1, B3:1/unit:1, B2:1/unit:1, B1:1/unit:1>>.
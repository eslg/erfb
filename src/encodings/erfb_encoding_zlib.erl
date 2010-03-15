%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc ZLib RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#zlib-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_zlib).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstream     :: zlib:zstream(),
                state       :: undefined | reading | writing,
                raw_state   :: term()}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() ->
    Z = zlib:open(),
    {ok, RawState} = erfb_encoding_raw:init(),
    {ok, #state{zstream     = Z,
                state       = undefined,
                raw_state   = RawState}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: binary(), Read :: <<_:32,_:_*8>>, Rest::binary(), #state{}}.
read(PF, Box, <<Length:4/unit:8, Bytes/binary>>, Socket,
     State = #state{zstream     = Z,
                    state       = ZState,
                    raw_state   = RawState}) ->
    ?DEBUG("ZLIB reader starting for ~p.  Length: ~p~n", [Box, Length]),
    case ZState of
        writing ->
            ok = zlib:deflateEnd(Z),
            ok = zlib:inflateInit(Z);
        reading ->
            void;
        undefined ->
            ok = zlib:inflateInit(Z)
    end,
    {RectBytes, Rest} =
        case bstr:len(Bytes) of
            L when L < Length ->
                {erfb_utils:complete(Bytes, Length, Socket, true), <<>>};
            _ ->
                {bstr:substr(Bytes, 1, Length), bstr:substr(Bytes, Length+1)}
        end,
    Decompressed = zlib:inflate(Z, RectBytes),
    %%NOTE: We're assuming that everything went fine and the raw reader wont
    %%      need to read more bytes from the socket.  If it needs that, then it
    %%      fails.  So, we don't give it the Socket.
    {ok, Data, _, _, NewRawState} =
        erfb_encoding_raw:read(PF, Box, bstr:bstr(Decompressed), Socket, RawState),
    {ok, Data,
     <<Length:4/unit:8, RectBytes/binary>>,
     Rest, State#state{raw_state= NewRawState,
                       state    = reading}};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("ZLIB reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

%% @hidden
-spec write(#session{}, #box{}, binary(), #state{}) -> {ok, <<_:32,_:_*8>>, #state{}} | {error, invalid_data, #state{}}.
write(Session, Box, Data,
      State = #state{zstream    = Z,
                     state      = ZState,
                     raw_state  = RawState}) ->
    case ZState of
        reading ->
            ok = zlib:inflateEnd(Z),
            ok = zlib:deflateInit(Z);
        writing ->
            void;
        undefined ->
            ok = zlib:deflateInit(Z)
    end,
    case erfb_encoding_raw:write(Session, Box, Data, RawState) of
        {ok, Uncompressed, NewRawState} ->
            FinalData = bstr:bstr(zlib:deflate(Z, Uncompressed, sync)), %%TODO: Just the last rect. should be sync flushed
            {ok, erfb_utils:build_string(FinalData),
             State#state{state      = writing,
                         raw_state  = NewRawState}};
        {error, invalid_data, NewRawState} ->
            {error, invalid_data, State#state{state     = writing,
                                              raw_state = NewRawState}}
    end.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(Reason, #state{zstream   = Z,
                         raw_state = RawState}) ->
    zlib:close(Z),
    erfb_encoding_raw:terminate(Reason, RawState).
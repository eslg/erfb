%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding ZLIB
-module(erfb_encoding_zlib).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstream     :: zlib:zstream(),
                state       :: undefined | reading | writing,
                raw_state   :: term()}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> integer().
code() -> 6.

-spec init() -> {ok, #state{}}.
init() ->
    Z = zlib:open(),
    {ok, RawState} = erfb_encoding_raw:init(),
    {ok, #state{zstream     = Z,
                state       = undefined,
                raw_state   = RawState}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
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
    {ok, Rect, _, _, NewRawState} =
        erfb_encoding_raw:read(PF, Box, bstr:bstr(Decompressed), undefined, RawState),
    {ok,
     Rect#rectangle{encoding = ?MODULE},
     <<Length:4/unit:8, RectBytes/binary>>,
     Rest, State#state{raw_state= NewRawState,
                       state    = reading}};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("ZLIB reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(PF, Box, Data,
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
    case erfb_encoding_raw:write(PF, Box, Data, RawState) of
        {ok, Uncompressed, NewRawState} ->
            FinalData = bstr:bstr(zlib:deflate(Z, Uncompressed, sync)), %%TODO: Just the last rect. should be sync flushed
            {ok, erfb_utils:build_string(FinalData),
             State#state{state      = writing,
                         raw_state  = NewRawState}};
        {error, invalid_data, NewRawState} ->
            {error, invalid_data, State#state{state     = writing,
                                              raw_state = NewRawState}}
    end.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, #state{zstream   = Z,
                         raw_state = RawState}) ->
    zlib:close(Z),
    erfb_encoding_raw:terminate(Reason, RawState).
%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding ZRLE
-module(erfb_encoding_zrle).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstream     :: zlib:zstream(),
                state       :: undefined | reading | writing}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> 16.
code() -> 16.

-spec init() -> {ok, #state{}}.
init() ->
    Z = zlib:open(),
    {ok, #state{zstream     = Z,
                state       = undefined}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, <<Length:4/unit:8, Bytes/binary>>, Socket,
     State = #state{zstream     = Z,
                    state       = ZState}) ->
    ?DEBUG("ZRLE reader starting for ~p.  Length: ~p~n", [Box, Length]),
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
    {ok,
     #rectangle{box     = Box,
                encoding= ?MODULE,
                data    = Decompressed},
     <<Length:4/unit:8, RectBytes/binary>>,
     Rest, State#state{state = reading}};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("ZRLE reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_PF, _Box, Data,
      State = #state{zstream    = Z,
                     state      = ZState}) ->
    case ZState of
        reading ->
            ok = zlib:inflateEnd(Z),
            ok = zlib:deflateInit(Z);
        writing ->
            void;
        undefined ->
            ok = zlib:deflateInit(Z)
    end,
    FinalData = bstr:bstr(zlib:deflate(Z, Data, sync)), %%TODO: Just the last rect. should be sync flushed
    {ok, erfb_utils:build_string(FinalData), State#state{state = writing}}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{zstream   = Z}) ->
    zlib:close(Z).
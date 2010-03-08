%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc ZRLE RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#zrle-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_zrle).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {zstream     :: zlib:zstream(),
                state       :: undefined | reading | writing}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() ->
    Z = zlib:open(),
    {ok, #state{zstream     = Z,
                state       = undefined}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: iolist(), Read::binary(), Rest::binary(), #state{}}.
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
    {ok, Decompressed,
     <<Length:4/unit:8, RectBytes/binary>>,
     Rest, State#state{state = reading}};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("ZRLE reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

%% @hidden
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

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{zstream   = Z}) ->
    zlib:close(Z).
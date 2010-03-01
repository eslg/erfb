%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding Cursor
-module(erfb_encoding_cursor).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> -239.
code() -> -239.

-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read(#pixel_format{bits_per_pixel = BPP},
     Box = #box{width = W, height = H},
     Bytes, Socket, State) ->
    PixLength = erlang:trunc(W * H * BPP / 8),
    MaskLength = erfb_utils:floor((W+7)/8) * H,
    Length = PixLength + MaskLength,
    ?DEBUG("Cursor reader starting for ~p.  Length: ~p~n", [Box, {PixLength, MaskLength}]),
    {Read, Rest} =
        case bstr:len(Bytes) of
            L when L < Length ->
                {erfb_utils:complete(Bytes, Length, Socket, true), <<>>};
            _ ->
                {bstr:substr(Bytes, 1, Length), bstr:substr(Bytes, Length+1)}
        end,
    ?DEBUG("Cursor reader Read:~n~p~n", [Read]),
    <<Pixels:PixLength/binary, Mask:MaskLength/binary>> = Read,
    {ok,
     #rectangle{box        = Box,
                encoding   = ?MODULE,
                data       = #cursor_data{pixels = Pixels,
                                          bitmask= Mask}},
     Read, Rest, State}.

-spec write(#pixel_format{}, #box{}, #cursor_data{}, #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(#pixel_format{bits_per_pixel = BPP},
      Box = #box{width = W, height = H},
      Data = #cursor_data{pixels = Pixels,
                          bitmask= Mask}, State) ->
    PixLength = erlang:trunc(W * H * BPP / 8),
    MaskLength = erfb_utils:floor((W+7)/8) * H,
    case {bstr:len(Pixels), bstr:len(Mask)} of
        {PixLength, MaskLength} ->
            {ok, <<Pixels:PixLength/binary, Mask:MaskLength/binary>>, State};
        L ->
            ?ERROR("Invalid data for cursor encoding:~n\tBPP: ~p~n\tBox: ~p~n\tData: ~p~n\tLength: ~p vs. ~p~n", [BPP, Box, Data, {PixLength, MaskLength}, L]),
            {error, invalid_data, State}
    end.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Cursor RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#cursor-pseudo-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_cursor).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

%% @hidden
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #cursor_data{}, Read::binary(), Rest::binary(), #state{}}.
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
     #cursor_data{pixels = Pixels,
                  bitmask= Mask},
     Read, Rest, State}.

%% @hidden
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

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
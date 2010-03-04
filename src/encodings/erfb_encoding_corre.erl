%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc CoRRE RFB Encoding implementation
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_corre).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> 4.
code() -> 4.

-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read(#pixel_format{bits_per_pixel = BPP}, Box,
     <<Count:4/unit:8, Bytes/binary>>, Socket, State) ->
    PixelSize = erlang:trunc(BPP / 8),
    Length = (PixelSize + 4) * Count + PixelSize,
    ?DEBUG("CoRRE reader starting for ~p.  Count: ~p / Length: ~p~n", [Box, Count, Length]),
    AllBytes =
        case bstr:len(Bytes) of
            L when L < Length ->
                erfb_utils:complete(Bytes, Length, Socket, true);
            _ ->
                Bytes
        end,
    <<Background:PixelSize/unit:8, Rest/binary>> = AllBytes,
    RectBytes = bstr:substr(Rest, 1, Length - PixelSize),
    Rects =
        [#rectangle{box     = #box{x = X,
                                   y = Y,
                                   width = W,
                                   height = H},
                    encoding= ?MODULE,
                    data    = Pixel} ||
                   <<Pixel:PixelSize/unit:8,
                     X:1/unit:8,
                     Y:1/unit:8,
                     W:1/unit:8,
                     H:1/unit:8>> <= RectBytes],
    {ok,
     #rectangle{box        = Box,
                encoding   = ?MODULE,
                data       = #rre_data{background = Background,
                                       rectangles = Rects}},
    <<Count:4/unit:8, Background:PixelSize/unit:8, RectBytes/binary>>,
    bstr:substr(AllBytes, Length + 1), State};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("CoRRE reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(#pixel_format{bits_per_pixel = BPP}, _Box,
      #rre_data{background = Background,
                rectangles = Rects}, State) ->
    PixelSize = erlang:trunc(BPP / 8),
    Count = erlang:length(Rects),
    RectBytes =
        << <<Pixel:PixelSize/unit:8,
             X:1/unit:8,
             Y:1/unit:8,
             W:1/unit:8,
             H:1/unit:8>> ||
           #rectangle{box     = #box{x = X,
                                     y = Y,
                                     width = W,
                                     height = H},
                      encoding= ?MODULE,
                      data    = Pixel} <- Rects >>,
    {ok, <<Count:4/unit:8, Background:PixelSize/unit:8, RectBytes/binary>>, State};
write(_PF, _, Data, State) ->
    ?ERROR("Invalid data for corre encoding:~p~n", [Data]),
    {error, invalid_data, State}.


-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
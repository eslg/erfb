%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Raw RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#raw-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_raw).
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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: binary(), Read::binary(), Rest::binary(), #state{}}.
read(#pixel_format{bits_per_pixel = BPP},
     Box = #box{width = W, height = H},
     Bytes, Socket, State) ->
    Length = erlang:trunc(W * H * BPP / 8),
    ?DEBUG("Raw reader starting for ~p.  Length: ~p~n", [Box, Length]),
    {RectBytes, Rest} =
        case bstr:len(Bytes) of
            L when L < Length ->
                {erfb_utils:complete(Bytes, Length, Socket, true), <<>>};
            _ ->
                {bstr:substr(Bytes, 1, Length), bstr:substr(Bytes, Length+1)}
        end,
    {ok, RectBytes, RectBytes, Rest, State}.

%% @hidden
-spec write(#session{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(#session{pixel_format = #pixel_format{bits_per_pixel = BPP}}, Box = #box{width = W, height = H},
      Data, State) ->
    Length = erlang:trunc(W * H * BPP / 8),
    case bstr:len(Data) of
        Length ->
            {ok, Data, State};
        L ->
            ?ERROR("Invalid data for raw encoding:~n\tBPP: ~p~n\tBox: ~p~n\tData: ~p~n\tLength: ~p vs. ~p~n", [BPP, Box, Data, Length, L]),
            {error, invalid_data, State}
    end.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
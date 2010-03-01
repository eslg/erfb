%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding RAW
-module(erfb_encoding_raw).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> 0.
code() -> 0.

-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
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
    {ok,
     #rectangle{box        = Box,
                encoding   = ?MODULE,
                data       = RectBytes},
     RectBytes, Rest, State}.

-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(#pixel_format{bits_per_pixel = BPP}, Box = #box{width = W, height = H},
      Data, State) ->
    Length = erlang:trunc(W * H * BPP / 8),
    case bstr:len(Data) of
        Length ->
            {ok, Data, State};
        L ->
            ?ERROR("Invalid data for raw encoding:~n\tBPP: ~p~n\tBox: ~p~n\tData: ~p~n\tLength: ~p vs. ~p~n", [BPP, Box, Data, Length, L]),
            {error, invalid_data, State}
    end.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
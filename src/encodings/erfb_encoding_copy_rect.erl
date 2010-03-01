%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding COPY_RECT
-module(erfb_encoding_copy_rect).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> 1.
code() -> 1.

-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, <<X:4/unit:8, Y:4/unit:8, Rest/binary>>, _Socket, State) ->
    ?DEBUG("CopyRect reader starting for ~p.  Result: {~p, ~p}~n", [Box, X, Y]),
    {ok,
     #rectangle{box        = Box,
                encoding   = ?MODULE,
                data       = {X, Y}},
     <<X:4/unit:8, Y:4/unit:8>>,
     Rest, State};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("CopyRect reader starting for ~p.  Not enough bytes.~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 8, Socket, true), Socket, State).


-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_PF, _Box, {X, Y}, State) ->
    {ok, <<X:4/unit:8, Y:4/unit:8>>, State};
write(_PF, _, Data, State) ->
    ?ERROR("Invalid data for copy_rect encoding:~p~n", [Data]),
    {error, invalid_data, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
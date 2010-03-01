%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc ERFB encoding DesktopSize
-module(erfb_encoding_desktop_size).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(erfb_encoding).

-export([code/0, init/0, read/5, write/4, terminate/2]).

-include("erfblog.hrl").
-include("erfb.hrl").

-record(state, {}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec code() -> -223.
code() -> -223.

-spec init() -> {ok, #state{}}.
init() -> {ok, #state{}}.

-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, #rectangle{}, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, Bytes, _Socket, State) ->
    ?DEBUG("Desktop Size reader starting for ~p.~n", [Box]),
    {ok,
     #rectangle{box        = Box,
                encoding   = ?MODULE,
                data       = undefined},
     <<>>, Bytes, State}.

-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_PF, _Box, undefined, State) ->
    {ok, <<>>, State};
write(_PF, _Box, Data, State) ->
    ?ERROR("Invalid data for desktop_size encoding:~p~n", [Data]),
    {error, invalid_data, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
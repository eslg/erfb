%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc CopyRect RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#copyrect-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_copy_rect).
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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: {integer(), integer()}, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, <<X:1/unit:16, Y:1/unit:16, Rest/binary>>, _Socket, State) ->
    ?DEBUG("CopyRect reader starting for ~p.  Result: {~p, ~p}~n\tRest: ~p~n", [Box, X, Y, Rest]),
    {ok, {X, Y}, <<X:1/unit:16, Y:1/unit:16>>, Rest, State};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("CopyRect reader starting for ~p.  Not enough bytes.~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 8, Socket, true), Socket, State).

%% @hidden
-spec write(#session{}, #box{}, term(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_Session, _Box, {X, Y}, State) ->
    {ok, <<X:1/unit:16, Y:1/unit:16>>, State};
write(_Session, _, Data, State) ->
    ?ERROR("Invalid data for copy_rect encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc DesktopSize RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#desktopsize-pseudo-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_desktop_size).
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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: undefined, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, Bytes, _Socket, State) ->
    ?DEBUG("Desktop Size reader starting for ~p.~n", [Box]),
    {ok, undefined, <<>>, Bytes, State}.

%% @hidden
-spec write(#session{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}} | {error, invalid_data, #state{}}.
write(_Session, _Box, undefined, State) ->
    {ok, <<>>, State};
write(_Session, _Box, Data, State) ->
    ?ERROR("Invalid data for desktop_size encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
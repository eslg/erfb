%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc RFB Encoding implementation for those pseudo-encodings that can't be received in a <em>rectangle</em>
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#jpeg-quality-level-pseudo-encoding">More Information about JPEG Quality Level</a>
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#compression-level-pseudo-encoding">More Information about Compression Level</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_no_data).
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
    ?ERROR("NO DATA reader called for ~p.~n", [Box]),
    {ok, undefined, <<>>, Bytes, State}.

%% @hidden
-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {error, invalid_data, #state{}}.
write(_PF, _Box, Data, State) ->
    ?ERROR("Invalid data for NO DATA encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
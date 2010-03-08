%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc DesktopSize RFB Encoding implementation
%%% @reference <a href="http://www.tigervnc.com/cgi-bin/rfbproto#desktopname-pseudo-encoding">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_desktop_name).
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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: binary(), Read :: <<_:32,_:_*8>>, Rest::binary(), #state{}}.
read(_PF, Box, <<Length:4/unit:8, Bytes/binary>>, Socket, State) ->
    ?DEBUG("DesktopName reader starting for ~p.  Length: ~p~n", [Box, Length]),
    {RectBytes, Rest} =
        case bstr:len(Bytes) of
            L when L < Length ->
                {erfb_utils:complete(Bytes, Length, Socket, true), <<>>};
            _ ->
                {bstr:substr(Bytes, 1, Length), bstr:substr(Bytes, Length+1)}
        end,
    {ok, RectBytes,
     <<Length:4/unit:8, RectBytes/binary>>, Rest, State};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("DesktopName reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 4, Socket, true), Socket, State).

%% @hidden
-spec write(#pixel_format{}, #box{}, binary(), #state{}) -> {ok, <<_:32,_:_*8>>, #state{}} | {error, invalid_data, #state{}}.
write(_PF, _Box, Data, State) ->
    {ok, erfb_utils:build_string(Data), State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{}) -> ok.
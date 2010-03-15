%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc WMVI RFB Encoding implementation
%%% @reference <a href="http://wiki.multimedia.cx/index.php?title=VMware_Video#WMVi_.28display_mode_change.29">More Information</a>
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding_wmvi).
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
-spec read(#pixel_format{}, #box{}, binary(), port(), #state{}) -> {ok, Data :: #pixel_format{}, Read::binary(), Rest::binary(), #state{}}.
read(_PF, Box, <<Read:16/binary-unit:8,
                 Rest/binary>>, _Socket, State) ->
    ?DEBUG("WMVi reader starting for ~p.~n", [Box]),
    <<PFBits:1/unit:8,
      PFDepth:1/unit:8,
      PFBigEndian:1/unit:8,
      PFTrueColour:1/unit:8,
      PFRedMax:2/unit:8,
      PFGreenMax:2/unit:8,
      PFBlueMax:2/unit:8,
      PFRedShift:1/unit:8,
      PFGreenShift:1/unit:8,
      PFBlueShift:1/unit:8,
      _PFPadding:3/unit:8>> = Read,
    PF = #pixel_format{bits_per_pixel = PFBits,
                       depth          = PFDepth,
                       big_endian     = PFBigEndian =/= ?FALSE,
                       true_colour    = PFTrueColour =/= ?FALSE,
                       red_max        = PFRedMax,
                       green_max      = PFGreenMax,
                       blue_max       = PFBlueMax,
                       red_shift      = PFRedShift,
                       green_shift    = PFGreenShift,
                       blue_shift     = PFBlueShift},
    {ok, PF, Read, Rest, State};
read(PF, Box, Bytes, Socket, State) ->
    ?DEBUG("WMVi reader starting for ~p.  Not enough bytes~n", [Box]),
    read(PF, Box, erfb_utils:complete(Bytes, 16, Socket, true), Socket, State).

%% @hidden
-spec write(#session{}, #box{}, binary(), #state{}) -> {ok, binary(), #state{}, #session{}} | {error, invalid_data, #state{}}.
write(Session = #session{width  = SW,
                         height = SH},
      #box{width = W,
           height = H},
      PF, State) when is_record(PF, pixel_format) ->
    {ok, erfb_utils:pf_to_binary(PF), State,
     Session#session{pixel_format = PF,
                     width = case W of
                                 all -> SW;
                                 W -> W
                             end,
                     height = case H of
                                  all -> SH;
                                  H -> H
                              end}};
write(_Session, _Box, Data, State) ->
    ?ERROR("Invalid data for desktop_size encoding:~p~n", [Data]),
    {error, invalid_data, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) -> ok.
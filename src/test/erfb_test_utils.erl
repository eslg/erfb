%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Test Utils for eRFB
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_test_utils).
-author('Fernando Benavides <fbenavides@novamens.com>').

-export([session/0, session/1, session/2]).

-include("erfb.hrl").

-spec session() -> #session{}.
session() ->
    session({3,8}).

-spec session({3, 3 | 7 | 8} | [none | {vnc, binary()}]) -> #session{}.
session(Version = {_, _}) ->
    session(Version, [none]);
session(Security) ->
    session({3,8}, Security).

-spec session({3, 3 | 7 | 8}, [none | {vnc, binary()}]) -> #session{}.
session(Version, Security) ->
    #session{version = Version,
             name = <<"Computer (user)">>,
             width = 800,
             height = 600,
             pixel_format = #pixel_format{bits_per_pixel = 32,
                                          depth = 24,
                                          big_endian = false,
                                          true_colour = true,
                                          red_max = 255,
                                          green_max = 255,
                                          blue_max = 255,
                                          red_shift = 16,
                                          green_shift = 8,
                                          blue_shift = 0},
             security = Security}.

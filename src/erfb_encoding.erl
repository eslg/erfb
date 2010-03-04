%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Encoding Behaviour
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_encoding).
-author('Fernando Benavides <fbenavides@novamens.com>').

%% ====================================================================
%% External functions
%% ====================================================================
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{code,     0},
     {init,     0},
     {read,     5},
     {write,    4},
     {terminate,2}];
behaviour_info(_Other) ->
    undefined.

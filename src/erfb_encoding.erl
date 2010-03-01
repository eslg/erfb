%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc Behaviour definition for erfb_encoding
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

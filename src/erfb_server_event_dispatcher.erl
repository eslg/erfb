%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Event Dispatcher for client events received by the connected server processes
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_server_event_dispatcher).
-author('Fernando Benavides <fbenavides@novamens.com>').

-export([start_link/0]).
-export([subscribe/2, subscribe_link/2, unsubscribe/2, subscriptions/0]).
-export([notify/1]).

-include("erfblog.hrl").
-include("erfb.hrl").

-type(event_handler() :: Module :: atom() | {Module :: atom(), Id :: term()}).
%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).

-spec subscribe_link(event_handler(), term()) -> ok | {'EXIT', term()} | term(). 
subscribe_link(EventHandler, InitArgs) ->
    gen_event:add_sup_handler(?MODULE, EventHandler, InitArgs).

-spec subscribe(event_handler(), term()) -> ok | {'EXIT', term()} | term(). 
subscribe(EventHandler, InitArgs) ->
    gen_event:add_handler(?MODULE, EventHandler, InitArgs).

-spec unsubscribe(event_handler(), term()) -> term() | {error, module_not_found} | {'EXIT', term()}.
unsubscribe(EventHandler, Args) ->
    gen_event:delete_handler(?MODULE, EventHandler, Args).

-spec subscriptions() -> [event_handler()].
subscriptions() ->
    gen_event:which_handlers(?MODULE).

-spec notify(client_event()) -> ok.
notify(Event) ->
    ?DEBUG("Notifying ~p~n", [element(1, Event)]),
    gen_event:notify(?MODULE, Event).
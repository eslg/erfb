%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc Event Dispatcher for Client Processes of the Novamens RFB Server
-module(erfb_client_event_dispatcher).
-author('Fernando Benavides <fbenavides@novamens.com>').

-export([start_link/0]).
-export([subscribe/2, subscribe_link/2, unsubscribe/1, subscriptions/0]).
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

-spec unsubscribe(event_handler()) -> term() | {error, module_not_found} | {'EXIT', term()}.
unsubscribe(EventHandler) ->
    gen_event:delete_handler(?MODULE, EventHandler).

-spec subscriptions() -> [event_handler()].
subscriptions() ->
    gen_event:which_handlers(?MODULE).

-spec notify(server_event()) -> ok.
notify(Event) ->
    ?DEBUG("Notifying ~p~n", [element(1, Event)]),
    gen_event:notify(?MODULE, Event).
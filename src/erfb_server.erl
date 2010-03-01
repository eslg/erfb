%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright 2009 Novamens S.R.L.
%%% @doc RFB Server Supervisor
-module(erfb_server).

-behaviour(supervisor).

-export([start_link/0, init/1, prep_stop/1]).

-include("erfblog.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec prep_stop(_) -> [any()].
prep_stop(State) ->
    ?INFO("Preparing to stop~n\tChildren: ~p~n", [supervisor:which_children(?MODULE)]),
    [Module:prep_stop(State) ||
       {_, _, supervisor, [Module]} <- supervisor:which_children(?MODULE),
       lists:member({prep_stop, 1}, Module:module_info(exports))].

-spec init([]) -> {ok, {{one_for_one, 1, 10}, [tuple(),...]}}.
init([]) ->
    Listener = {erfb_server_listener_manager, 
                {erfb_server_listener_manager, start_link, []},
                permanent, 5000, supervisor, [erfb_server_listener_manager]},
    Dispatcher  = {erfb_server_event_dispatcher,
                   {erfb_server_event_dispatcher, start_link, []},
                   permanent, 5000, worker, [erfb_server_event_dispatcher]},
    Manager  = {erfb_server_manager,
                {erfb_server_manager, start_link, []},
                permanent, 5000, supervisor, [erfb_server_manager]},
    {ok, {{one_for_one, 1, 10}, [Listener, Dispatcher, Manager]}}.

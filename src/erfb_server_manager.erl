%% @author Fernando Benavides <fbenavides@novamens.com>
%% @copyright 2009 Novamens S.R.L.
%% @doc Server process manager for the RFB Server  
-module(erfb_server_manager).

-behaviour(supervisor).

-export([start_link/0, start_server/2, init/1, prep_stop/1]).

-include("erfblog.hrl").
-include("erfb.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> any().
start_link() -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_server(#session{}, [atom()]) -> {ok, pid() | undefined} | {error, term()}.
start_server(Session, Encodings) ->
    supervisor:start_child(?MODULE, [Session, Encodings]).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    ?INFO("Preparing to stop~n\tChildren: ~p~n", [supervisor:which_children(?MODULE)]),
    [Module:prep_stop(Pid, State) ||
       {_, Pid, _, [Module]} <- supervisor:which_children(?MODULE),
       lists:member({prep_stop, 2}, Module:module_info(exports))].

-spec init([]) -> {ok, {{simple_one_for_one, 100, 1}, [{undefined, {erfb_server, start_link, []}, temporary, 5000, worker, [erfb_server]}]}}.
init([]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, 100, 1},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {erfb_server_process, start_link, []},   % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  5000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [erfb_server_process]                    % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fbenavides@novamens.com>
%%% @copyright (C) 2010 Novamens S.A.
%%% @doc Listener process for RFB clients
%%% @reference See <a href="http://www.trapexit.org/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles">this article</a> for more information
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erfb_client_listener).
-author('Fernando Benavides <fbenavides@novamens.com>').

-behaviour(gen_server).

%% @headerfile "erfb.hrl"
-include("erfb.hrl").

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------
-include("erfblog.hrl").

-record(state, {
                listener :: port(), % Listening socket
                acceptor :: term()  % Asynchronous acceptor's internal reference
               }).

%% ====================================================================
%% External functions
%% ====================================================================

%% @spec start_link(ip(), integer(), integer()) -> {ok, pid()}
%% @doc  Starts a new client listener
-spec start_link(ip(), integer(), integer()) -> {ok, pid()}.
start_link(Ip, Port, Backlog) -> 
    ?INFO("Starting RFB Client Listener...~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Ip, Port, Backlog}, []).

%% ====================================================================
%% Callback functions
%% ====================================================================
%% @hidden
-spec init({ip(), integer(), integer()}) -> {ok, #state{}} | {stop, term()}.
init({Ip, Port, Backlog}) ->
    process_flag(trap_exit, true),
    Opts = [binary,
            {backlog, Backlog},
            {ip, Ip},
            {reuseaddr, true},
            {keepalive, true},
            {packet, 0},
            {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Socket} ->
            %%Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Socket, -1),
            {ok, #state{listener = Socket,
                        acceptor = Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @hidden
-spec handle_call(any(), any(), #state{}) -> {stop, {unknown_call, any()}, #state{}}.
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%% @hidden
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener = ListSock, acceptor = Ref} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok ->
                void;
            {error, Reason} ->
                exit({set_sockopt, Reason})
        end,
        
        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, Pid} = erfb_client_manager:start_client(),
        gen_tcp:controlling_process(CliSocket, Pid),
        
        %% Instruct the new FSM that it owns the socket.
        erfb_client_process:set_socket(Pid, CliSocket),
        
        %% Signal the network driver that we are ready to accept another connection
        NewRef =
            case prim_inet:async_accept(ListSock, -1) of
                {ok, NR} ->
                    NR;
                {error, Err} ->
                    exit({async_accept, inet:format_error(Err)})
            end,
        
        {noreply, State#state{acceptor=NewRef}}
    catch
        exit:Error ->
            ?ERROR("Error in async accept: ~p.\n", [Error]),
            {stop, Error, State}
    end;
handle_info({inet_async, ListSock, Ref, Error},
            #state{listener = ListSock, acceptor = Ref} = State) ->
    ?ERROR("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
    ?INFO("RFB listener terminated~n", []),
    ok.

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc Taken from prim_inet.  We are merely copying some socket options from the
%%      listening socket to the new client socket.
-spec set_sockopt(port(), port()) -> ok | {error, Reason :: term()}.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok ->
                    ok;
                Error ->
                    gen_tcp:close(CliSocket),
                    Error
            end;
        Error ->
            gen_tcp:close(CliSocket),
            Error
    end.
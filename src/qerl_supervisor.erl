-module(qerl_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Args) -> supervisor:start_link(qerl_supervisor, [7000,10]).
init([ListeningPort,InitialNoOfConnections]) ->
    {ok, {{one_for_one, 1, 60},
            [{qerl_conn_manager, {qerl_conn_manager, start_link, [qerl_conn_listener, ListeningPort, InitialNoOfConnections]},
                    permanent, brutal_kill, worker, [qerl_conn_listener]}]}};
init(Args) -> io:format("Wrong initializion params were passed: ~p~n",[Args]).


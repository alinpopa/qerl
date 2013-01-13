-module(qerl_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(_Args) ->
  supervisor:start_link({local, qerl_supervisor}, qerl_supervisor, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxTimeBetRestarts = 3600,
    SupervisorFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
    ConnectionManagerSpec = {qerl_conn_manage, {qerl_conn_manager, start_link, [7000, 10]},
                             permanent, brutal_kill, worker, []},
    SessionManagerSpec = {qerl_session_manager, {qerl_session_manager, start_link, [7000]},
                          permanent, brutal_kill, worker, []},
    MemoryQueueSpec = {qerl_memory_queue, {qerl_memory_queue, start_link, []},
                       permanent, brutal_kill, worker, []},
    ChildSpecs = [ConnectionManagerSpec, SessionManagerSpec, MemoryQueueSpec],
    {ok, {SupervisorFlags, ChildSpecs}}.


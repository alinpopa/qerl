-module(qerl_stomp_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start/1, start_link/1, stop/1]).
-export(['READY'/2]).

start(Args) -> gen_fsm:start(?MODULE, Args, []).

start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).

init(_Args) -> {ok, 'READY', []}.

'READY'({stop}, StateData) ->
    io:format("Stop the fsm module~n"),
    {stop, normal, StateData};
'READY'(_Event, StateData) ->
    {stop, unimplemented, StateData}.

stop(Pid) ->
    gen_fsm:send_event(Pid, {stop}).

handle_event(_Event, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

handle_info(_Info, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


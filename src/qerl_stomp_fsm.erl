-module(qerl_stomp_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start/1, start_link/1, stop/1, process/2]).
-export(['READY'/2, 'CONNECTED'/2]).

-record(state,{parent}).

start(Args) -> gen_fsm:start(?MODULE, Args, []).

start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).

init(Args) ->
    [Parent] = Args,
    gen_server:cast(Parent,{msg_from_fsm}),
    io:format("Args: ~p~n",[Args]),
    {ok, 'READY', Args}.

process(FsmPid, []) ->
    gen_fsm:send_event(FsmPid, {nothing});
process(FsmPid, [H|T]) ->
    gen_fsm:send_event(FsmPid,H),
    process(FsmPid,T).

'READY'({stop}, StateData) ->
    log("STOP"),
    {stop, normal, StateData};
'READY'({connect,{headers,Headers}}, StateData) ->
    log("CONNECT"),
    {next_state, 'CONNECTED', StateData};
'READY'(Event, StateData) ->
    {next_state, 'READY', StateData}.

'CONNECTED'({stop},StateData) ->
    log("STOP"),
    {stop, normal, StateData};
'CONNECTED'(Event,StateData) ->
    log("Connected and got normal event"),
    {next_state, 'CONNECTED', StateData}.

stop(Pid) -> gen_fsm:send_event(Pid, {stop}).

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

log(Msg) -> 
    io:format("gen_fsm: ~p~n",[Msg]).


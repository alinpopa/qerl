-module(qerl_stomp_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/1, stop/1, process/2]).
-export(['READY'/2, 'CONNECTED'/2]).

-record(state,{parent,current_session}).

start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).

init(Args) ->
    [Parent] = Args,
    gen_server:cast(Parent,{msg_from_fsm}),
    {ok, 'READY', #state{parent=Parent}}.

process(FsmPid, Else) ->
    gen_fsm:send_event(FsmPid,Else),
    ok.

'READY'({stop}, StateData) ->
    trace("STOP"),
    {stop, normal, StateData};
'READY'({connect,{headers,Headers}}, StateData) ->
    trace("CONNECT"),
    io:format("Headers: ~p~n",[Headers]),
    SessionId = qerl_session_manager:gen_session(),
    send_to_client(StateData#state.parent,"CONNECTED\nsession:" ++ SessionId),
    {next_state, 'CONNECTED', StateData#state{current_session = SessionId}};
'READY'(Event, StateData) ->
    trace(Event),
    [ParentListener] = StateData,
    send_to_client(ParentListener,"ERROR\nNot connected"),
    {next_state, 'READY', StateData}.

'CONNECTED'({stop},StateData) ->
    trace("STOP"),
    {stop, normal, StateData};
'CONNECTED'({send,{headers,Headers},{body,Body}},StateData) ->
    trace("SEND"),
    io:format("Headers: ~p~n",[Headers]),
    io:format("Body: ~p~n",[Body]),
    {next_state, 'CONNECTED', StateData};
'CONNECTED'({disconnect,{headers,Headers}},StateData) ->
    trace("DISCONNECT"),
    trace(StateData),
    io:format("Headers: ~p~n",[Headers]),
    qerl_conn_listener:stop(StateData#state.parent),
    {stop, normal, StateData};
'CONNECTED'(Event,StateData) ->
    trace(Event),
    {next_state, 'CONNECTED', StateData}.

stop(Pid) -> gen_fsm:send_event(Pid, {stop}).

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

trace(Msg) -> io:format("~p: ~p~n",[?MODULE,Msg]).
send_to_client(Parent,Msg) -> qerl_conn_listener:send_to_client(Parent,Msg).


-module(qerl_stomp_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/1, stop/1, process/2]).
-export(['READY'/2, 'CONNECTED'/2]).

-record(state,{parent,current_session}).

-define(CONN_LISTENER,qerl_conn_listener).
-define(QSERVER,qerl_memory_queue).
-define(SESSION_MANAGER,qerl_session_manager).

%%
%% API functions
%%
start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).

process(FsmPid, Else) ->
  gen_fsm:send_event(FsmPid,Else),
  ok.

'READY'({stop}, StateData) ->
  trace("STOP"),
  {stop, normal, StateData};
'READY'({connect,{headers,Headers}}, StateData) ->
  trace("CONNECT"),
  io:format("Headers: ~p~n",[Headers]),
  SessionId = ?SESSION_MANAGER:gen_session(),
  send_to_client(StateData#state.parent,"CONNECTED\nsession:" ++ SessionId),
  {next_state, 'CONNECTED', StateData#state{current_session = SessionId}};
'READY'({unknown_command,UnknownCommand}, StateData) ->
  send_to_client(StateData#state.parent,"ERROR\nmessage:Unknown STOMP action: " ++ UnknownCommand),
  {next_state, 'READY', StateData};
'READY'(_Event, StateData) ->
  ParentListener = StateData#state.parent,
  send_to_client(ParentListener,"ERROR\nNot connected"),
  {next_state, 'READY', StateData}.

'CONNECTED'({stop},StateData) ->
  trace("STOP"),
  {stop, normal, StateData};
'CONNECTED'({send,{headers,Headers},{body,Body}},StateData) ->
  trace("SEND"),
  io:format("Headers: ~p~n",[Headers]),
  io:format("Body: ~p~n",[Body]),
  ?QSERVER:produce(Headers,Body),
  {next_state, 'CONNECTED', StateData};
'CONNECTED'({queue_info},StateData) ->
  QInfo = ?QSERVER:info(),
  send_to_client(StateData#state.parent, format_queue_info(QInfo)),
  {next_state, 'CONNECTED', StateData};
'CONNECTED'({subscribe,_},StateData) ->
  case ?QSERVER:consume() of
    {consume,Message} ->
      {{message,MessageBody},{_,_}} = Message,
      send_to_client(StateData#state.parent, "MESSAGE\ndestination:/x/y/z\nmessage-id: <message-identifier>\n\n" ++ MessageBody);
    {error,Error} ->
      send_to_client(StateData#state.parent, "ERROR\n" ++ Error)
  end,
  {next_state, 'CONNECTED', StateData};
'CONNECTED'({disconnect,{headers,Headers}},StateData) ->
  trace("DISCONNECT"),
  trace(StateData),
  io:format("Headers: ~p~n",[Headers]),
  ?CONN_LISTENER:stop(StateData#state.parent),
  {stop, normal, StateData};
'CONNECTED'({unknown_command,UnknownCommand}, StateData) ->
  send_to_client(StateData#state.parent,"ERROR\nmessage:Unknown STOMP action: " ++ UnknownCommand),
  {next_state, 'CONNECTED', StateData};
'CONNECTED'(Event,StateData) ->
  trace(Event),
  {next_state, 'CONNECTED', StateData}.

stop(Pid) -> gen_fsm:send_event(Pid, {stop}).
trace(Msg) -> io:format("~p: ~p~n",[?MODULE,Msg]).
send_to_client(Parent,Msg) -> ?CONN_LISTENER:send_to_client(Parent,Msg).

format_queue_info(QueueInfo) ->
  lists:flatten(io_lib:format("~p",[QueueInfo])).

%%
%% Callback functions
%%
init(Args) ->
  [Parent] = Args,
  {ok, 'READY', #state{parent=Parent}}.

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) ->
  ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


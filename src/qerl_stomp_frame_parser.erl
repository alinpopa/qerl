-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0,parse/2,stop/1]).
-export(['NEW'/3,'WAITING'/3]).

-import(binary, [replace/4]).
-import(qerl_stomp_utils, [drop/2]).

-record(state,{frames=[], blank_line = false, current_lines=[], last_data = <<>>, uncomplete_frame = <<>>}).

-define(NULL,0).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

stop(ParserPid) ->
  gen_fsm:send_all_state_event(ParserPid,stop).

init([]) -> {ok, 'NEW', #state{}}.

'NEW'({parse,Data}, _From, State) ->
  case drop_all(Data) of
    <<>> ->
      {reply,{new,[]},'NEW',State};
    _ ->
      ExistingLines = State#state.current_lines,
      {reply,{waiting,ExistingLines},'WAITING',State#state{current_lines = [Data], last_data = Data}}
  end.

'WAITING'({parse,Data}, _From, State) ->
  ExistingLines = State#state.current_lines,
  LastData = State#state.last_data,
  UncompleteFrame = State#state.uncomplete_frame,
  io:format("Last data: ~p~n",[LastData]),
  io:format("Parse data: ~p~n",[parse(Data)]),
  CurrentLines = [Data|ExistingLines],
  {reply, {waiting, CurrentLines}, 'WAITING',
    State#state{current_lines = CurrentLines,
                last_data = Data}
  }.

handle_event(stop, _StateName, State) ->
  {stop, normal, State};
handle_event(_Event, _StateName, StateData) ->
  {stop, unimplemented, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) -> {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% Drop all bytes like LF and NULL
drop_all(<<>>) -> <<>>;
drop_all(Bin) -> drop_all(Bin,[
      fun(X) -> drop(lf,X) end,
      fun(X) -> drop(null,X) end
    ]).
drop_all(<<>>,_FunctionsToApply) -> <<>>;
drop_all(Bin, []) -> Bin;
drop_all(Bin, [FunctionToApply|RestOfFunctionsToApply]) ->
  drop_all(FunctionToApply(Bin), RestOfFunctionsToApply).

parse(<<>>) -> {<<>>,<<>>};
parse(Data) ->
  case binary:split(Data,<<?NULL>>,[trim]) of
    [Element] -> {Element,<<>>};
    [Element,Rest] -> {Element,Rest}
  end.


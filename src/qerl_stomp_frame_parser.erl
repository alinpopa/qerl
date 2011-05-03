-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0,parse/2,stop/1]).
-export(['NEW'/3,'WAITING'/3]).

-import(binary, [replace/4]).
-import(qerl_stomp_utils, [drop/2]).

-record(state,{frames=[], uncomplete_data = <<>>}).

-define(NULL,0).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

stop(ParserPid) ->
  gen_fsm:send_all_state_event(ParserPid,stop).

init([]) -> {ok, 'NEW', #state{}}.

'NEW'({parse,Data}, _From, State) ->
  case drop_all(Data) of
    <<>> -> {reply,{new,[]},'NEW',State};
    _ ->
      ExistingFrames = State#state.frames,
      DataToProcess = drop(cr,Data),
      {reply,{waiting,ExistingFrames},'WAITING',State#state{frames = [DataToProcess|ExistingFrames], uncomplete_data = Data}}
  end.

'WAITING'({parse,Data}, _From, State) ->
  ExistingFrames = State#state.frames,
  UncompleteData = State#state.uncomplete_data,
  io:format("Uncomplete data: ~p~n",[UncompleteData]),
  DataToProcess = drop(cr,Data),
  {reply, {waiting, ExistingFrames}, 'WAITING',
    State#state{frames = lists:reverse([DataToProcess|ExistingFrames]), uncomplete_data = <<UncompleteData/binary,Data/binary>>}
  }.

handle_event(stop, _StateName, State) ->
  {stop, normal, State};
handle_event(_Event, _StateName, StateData) ->
  {stop, unimplemented, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) -> {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% Drop all bytes like CR, LF and NULL
drop_all(<<>>) -> <<>>;
drop_all(Bin) -> drop_all(Bin,[
      fun(X) -> drop(lf,X) end,
      fun(X) -> drop(null,X) end
    ]).
drop_all(<<>>,_FunctionsToApply) -> <<>>;
drop_all(Bin, []) -> Bin;
drop_all(Bin, [FunctionToApply|RestOfFunctionsToApply]) ->
  drop_all(FunctionToApply(Bin), RestOfFunctionsToApply).


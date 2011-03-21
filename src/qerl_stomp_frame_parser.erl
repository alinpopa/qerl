-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0,parse/2,stop/1]).
-export(['NEW'/3,'WAITING'/3]).

-record(state,{frames=[], uncomplete_data = <<>>}).

-define(LF,10).
-define(NULL,0).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

stop(ParserPid) ->
  gen_fsm:send_all_state_event(ParserPid,stop).

init([]) -> {ok, 'NEW', #state{}}.

'NEW'({parse,Data}, _From, State) ->
  case Data of
    <<?LF>> -> {reply,{new,[]},'NEW',State};
    _ ->
      ExistingFrames = State#state.frames,
      {reply,{waiting,ExistingFrames},'WAITING',State#state{frames = [Data|ExistingFrames], uncomplete_data = Data}}
  end.

'WAITING'({parse,Data}, _From, State) ->
  ExistingFrames = State#state.frames,
  UncompleteData = State#state.uncomplete_data,
  io:format("Uncomplete data: ~p~n",[UncompleteData]),
  {reply, {waiting, ExistingFrames}, 'WAITING',
    State#state{frames = lists:reverse([Data|ExistingFrames]), uncomplete_data = <<UncompleteData/binary,Data/binary>>}
  }.

handle_event(stop, _StateName, State) ->
  {stop, normal, State};
handle_event(_Event, _StateName, StateData) ->
  {stop, unimplemented, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) -> {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


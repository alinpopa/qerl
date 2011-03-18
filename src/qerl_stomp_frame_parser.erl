-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0]).
-export([new/3,parse/2]).

-record(state,{frames=[]}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

new({parse,Data},_From,State) ->
  ExistingFrames = State#state.frames,
  {reply,{ok,ExistingFrames},new,#state{frames = [Data|ExistingFrames]}}.

init([]) -> {ok,new,#state{}}.

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, StateName, StateData) ->
  io:format("Existing state: ~p; data: ~p~n",[StateName,StateData]),
  {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0,parse/2]).
-export(['WAITING'/3]).

-import(binary, [bin_to_list/1, match/2, replace/4, split/2, split/3]).

-record(state,{frames=[], uncomplete_data = <<>>, is_null = false}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

'WAITING'({parse,Data}, _From, State) ->
  ExistingFrames = State#state.frames,
  UncompleteData = State#state.uncomplete_data,
  io:format("Is uncomplete data: ~p~n",[is_null(UncompleteData)]),
  io:format("Is first byte null: ~p~n",[is_first_byte_null(Data)]),
  io:format("Existing byte null: ~p~n",[State#state.is_null]),
  {reply, {waiting, ExistingFrames}, 'WAITING',
    #state{
            frames = [Data|ExistingFrames],
            uncomplete_data = <<Data/binary,UncompleteData/binary>>,
            is_null = is_null(Data)
        }
  }.

init([]) -> {ok, 'WAITING', #state{}}.

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, StateName, StateData) ->
  io:format("Existing state: ~p; data: ~p~n",[StateName,StateData]),
  {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

is_first_byte_null(<<>>) -> false;
is_first_byte_null(UncompleteData) ->
  case binary:first(UncompleteData) of
    0 -> true;
    _ -> false
  end.

is_null(UncompleteData) ->
  case match(UncompleteData,<<0>>) of
      nomatch -> false;
      _ -> true
    end.


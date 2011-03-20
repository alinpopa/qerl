-module(qerl_stomp_frame_parser).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/0,parse/2]).
-export(['NEW'/3,'WAITING'/3]).

-import(binary, [bin_to_list/1, match/2, replace/4, split/2, split/3]).

-record(state,{frames=[], uncomplete_data = <<>>}).

-define(LF,10).
-define(CR,13).
-define(NULL,0).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

parse(ParserPid,Data) ->
  gen_fsm:sync_send_event(ParserPid,{parse,Data}).

'NEW'({parse,Data}, _From, State) ->
  case drop(cr,Data) of
    <<?LF>> -> {reply,{new,[]},'NEW',State};
    _ ->
      ExistingFrames = State#state.frames,
      {reply,{waiting,ExistingFrames},'WAITING',#state{frames = [Data|ExistingFrames], uncomplete_data = drop(cr,Data)}}
  end.

'WAITING'({parse,Data}, _From, State) ->
  ExistingFrames = State#state.frames,
  UncompleteData = State#state.uncomplete_data,
  NoCrData = drop(cr,Data),
  io:format("Uncomplete data: ~p~n",[UncompleteData]),
  {reply, {waiting, ExistingFrames}, 'WAITING',
    #state{frames = lists:reverse([Data|ExistingFrames]), uncomplete_data = <<UncompleteData/binary,NoCrData/binary>>}
  }.

init([]) -> {ok, 'NEW', #state{}}.

handle_event(_Event, _StateName, StateData) -> {stop, unimplemented, StateData}.
handle_sync_event(_Event, _From, StateName, StateData) ->
  io:format("Existing state: ~p; data: ~p~n",[StateName,StateData]),
  {next_state,new,StateData}.
handle_info(_Info, _StateName, StateData) -> {stop, unimplemented, StateData}.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

drop(_,<<>>) -> <<>>;
drop(What,Bin) when is_binary(Bin) ->
    case What of
        cr -> replace(Bin,<<?CR>>,<<>>,[global]);
        null -> replace(Bin,<<?NULL>>,<<>>,[global]);
        _ -> Bin
    end;
drop(_What,_Bin) -> <<>>.


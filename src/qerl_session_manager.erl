-module(qerl_session_manager).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,gen_session/0,get_sessions/0,delete_session/1]).

-record(session_state,{active_sessions=[],last_session_index=0}).

%%
%% API functions
%%
start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
gen_session() -> gen_server:call(whereis(?MODULE), {generate,session}).
get_sessions() -> gen_server:call(whereis(?MODULE), {get,sessions}).
delete_session(SessionId) -> gen_server:call(whereis(?MODULE), {delete,session,SessionId}).

remove_element(_,[],Accumulator) -> Accumulator;
remove_element(Element,[H|T],Accumulator) ->
  case Element =:= H of
    true -> remove_element(Element,T,Accumulator);
    _ -> remove_element(Element,T,[H|Accumulator])
  end.

%%
%% Callback functions
%%
init(_Args) -> {ok,#session_state{}}.

handle_call({generate,session},_From,State) ->
  LastSessionIndex = State#session_state.last_session_index + 1,
  SessionId = "CLIENT_SESSION:" ++ lists:flatten(io_lib:format("~p",[LastSessionIndex])),
  ActiveSessions = [SessionId | State#session_state.active_sessions],
  {reply,SessionId,State#session_state{active_sessions = ActiveSessions, last_session_index = LastSessionIndex}};
handle_call({delete,session,Id},_From,State) ->
  ActiveSessions = State#session_state.active_sessions,
  {reply,ok,State#session_state{active_sessions = remove_element(Id,ActiveSessions,[])}};
handle_call({get,sessions},_From,State) -> {reply,State,State};
handle_call(_Request,_From,State) -> {noreply,State}.

handle_cast(_Request,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


-module(qerl_session_manager).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/1,gen_session/0,get_sessions/0,delete_session/1]).

-record(session_state,{active_sessions=[],last_session_index=0,tcp_listening_port}).

%%
%% API functions
%%
start_link(ListeningPort) -> gen_server:start_link({local,?MODULE},?MODULE,[ListeningPort],[]).
gen_session() -> gen_server:call(whereis(?MODULE), {generate,session}).
get_sessions() -> gen_server:call(whereis(?MODULE), {get,sessions}).
delete_session(SessionId) -> gen_server:call(whereis(?MODULE), {delete,session,SessionId}).

remove_session(_,[],Sessions) -> lists:reverse(Sessions);
remove_session(SessionId,[H|T],Sessions) when SessionId =:= H -> remove_session(SessionId,T,Sessions);
remove_session(SessionId,[H|T],Sessions) -> remove_session(SessionId,T,[H|Sessions]).

hostname() ->
    {ok,Hostname} = inet:gethostname(),
    Hostname.

format_and_flat(NonStringData) ->
    lists:flatten(io_lib:format("~p",[NonStringData])).

timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    lists:flatten(io_lib:format("~p~p~p",[Mega,Sec,Micro])).

%%
%% Callback functions
%%
init([TcpListeningPort]) ->
    {ok,#session_state{tcp_listening_port = TcpListeningPort}};
init(_Args) ->
    {ok,#session_state{}}.

handle_call({generate,session},_From,State) ->
  LastSessionIndex = State#session_state.last_session_index + 1,
  TcpListeningPort = State#session_state.tcp_listening_port,
  SessionId = "ID" ++
              ":" ++ hostname() ++
              ":" ++ format_and_flat(TcpListeningPort) ++
              ":" ++ timestamp() ++
              ":" ++ format_and_flat(LastSessionIndex),
  ActiveSessions = [SessionId | State#session_state.active_sessions],
  {reply,SessionId,State#session_state{active_sessions = ActiveSessions, last_session_index = LastSessionIndex}};
handle_call({delete,session,Id},_From,State) ->
  ActiveSessions = State#session_state.active_sessions,
  {reply,ok,State#session_state{active_sessions = remove_session(Id,ActiveSessions,[])}};
handle_call({get,sessions},_From,State) -> {reply,State,State};
handle_call(_Request,_From,State) -> {noreply,State}.

handle_cast(_Request,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


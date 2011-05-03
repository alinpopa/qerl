-module(qerl_tcp_filters).
-behaviour(gen_server).
-export([start_link/0,apply/2,stop/1]).
-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).

-import(qerl_stomp_utils, [drop/2]).

-define(FILTERS,[
  fun(Data) -> drop(cr,Data) end
]).

start_link() -> gen_server:start_link(?MODULE,[],[]).

apply(TcpFiltersPid,Data) ->
  {ok, FilteredData} = gen_server:call(TcpFiltersPid,{apply,Data}),
  FilteredData.

stop(TcpFiltersPid) ->
  gen_server:cast(TcpFiltersPid, stop).

init([]) -> {ok, []}.

handle_call({apply,Data},_From,State) ->
  FilteredData = lists:foldl(fun(F,Data) -> F(Data) end, Data, ?FILTERS),
  {reply,{ok,FilteredData},State};
handle_call(_Request,_From,State) -> {noreply,State}.

handle_cast(stop,State) ->
  {stop, normal, State};
handle_cast(_Request,State) ->
  {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


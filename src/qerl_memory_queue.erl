-module(qerl_memory_queue).
-behaviour(gen_server).
-behaviour(qerl_queue).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,produce/2,consume/0,info/0]).

-record(state,{queue}).

%%
%% API functions
%%
start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
produce(Headers,Message) -> gen_server:call(?MODULE,{produce,Headers,Message}).
consume() -> gen_server:call(?MODULE,{consume}).
info() -> gen_server:call(?MODULE,{info}).

%%
%% Callback functions
%%
init([]) -> {ok, #state{queue = queue:new()}}.

handle_call({produce,Headers,Message},_From,State) ->
  NewQ = queue:in({{message,Message},{headers,Headers}},State#state.queue),
  {reply,ok,State#state{queue = NewQ}};
handle_call({consume},_From,State) ->
  case queue:out(State#state.queue) of
    {{value,Message},NewQ} -> {reply,{consume,Message},State#state{queue = NewQ}};
    {empty,_} -> {reply,{error,"No message"},State}
  end;
handle_call({info},_From,State) ->
  {reply,queue:to_list(State#state.queue),State};
handle_call(_Request,_From,State) -> {noreply,State}.

handle_cast(_Request,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


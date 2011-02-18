-module(qerl_conn_manager).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/3,detach/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-record(server_state,{module,socket,listeners_size}).

%%
%% API functions
%%
start_link(ListeningModule,Port,ListenersSize) ->
  gen_server:start_link({local,?MODULE},?MODULE,[ListeningModule,Port,ListenersSize],[]).

detach() -> gen_server:call(?MODULE,detach).

spawn_listeners(_,_,0) -> ok;
spawn_listeners(ListeningModule,ListeningSocket,ListenersSize) ->
    ListeningModule:start_link(?MODULE,ListeningSocket),
    spawn_listeners(ListeningModule,ListeningSocket,ListenersSize-1).

%%
%% Callback functions
%%
init([ListeningModule,Port,ListenersSize]) ->
    case gen_tcp:listen(Port,?TCP_OPTIONS) of
        {ok,ListeningSocket} ->
            ok = spawn_listeners(ListeningModule,ListeningSocket,ListenersSize),
            io:format(" >> Server started successful on port: ~p, listeners size: ~p~n",[Port,ListenersSize]),
            {ok,#server_state{module=ListeningModule,socket=ListeningSocket,listeners_size=ListenersSize}};
        {error,Reason} ->
            {stop,Reason}
    end.

handle_call(detach,{From,_Ref}, State = #server_state{module=ListeningModule,socket=ListeningSocket}) ->
    ok = spawn_listeners(ListeningModule,ListeningSocket,1),
    unlink(From),
    {reply,ok,State};
handle_call(stop,_From,State) -> {stop,normal,State};
handle_call(_Request,_From,State) ->
    Reply = ok,
    {reply,Reply,State}.

handle_cast(_Msg,State) -> {noreply,State}.
handle_info(Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


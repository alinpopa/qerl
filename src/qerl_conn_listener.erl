-module(qerl_conn_listener).
-behaviour(gen_server).

-compile(export_all).

start_link(LSocket) ->
    io:format(" -> ~p:start_link >> LSocket - ~p~n",[?MODULE,LSocket]),
    gen_server:start_link(?MODULE,[LSocket],[]).

init([LSocket]) ->
    io:format(" -> ~p:init >> LSocket - ~p~n",[?MODULE,LSocket]),
    gen_server:cast(self(), {listen, LSocket}),
    {ok,[]}.

handle_cast({listen,LSocket}, []) ->
    {ok,ClientSocket} = gen_tcp:accept(LSocket),
    gen_tcp:controlling_process(ClientSocket,self()),
    inet:setopts(ClientSocket, [{active, once}]),
    qerl_conn_manager:detach(),
    {noreply,{ClientSocket}};
handle_cast(_Msg,State) ->
    {noreply,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_info({tcp,ClientSocket,Bin},State) ->
    io:format(" -> qerl_conn_listener:handle_info >> Info - {tcp,~p,~p}, State - ~p~n",[ClientSocket,Bin,State]),
    inet:setopts(ClientSocket, [{active, once}]),
    {noreply,State};
handle_info({tcp_closed,ClientSocket},State) -> {stop,normal,State};
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


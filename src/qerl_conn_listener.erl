-module(qerl_conn_listener).
-behaviour(gen_server).

-compile(export_all).

start_link(ConnMngModule,LSocket) -> gen_server:start_link(?MODULE,[ConnMngModule,LSocket],[]).

init([ConnMngModule,LSocket]) ->
    process_flag(trap_exit,true),
    gen_server:cast(self(), {listen,ConnMngModule,LSocket}),
    {ok,[]}.

handle_cast({listen,ConnMngModule,LSocket}, []) ->
    {ok,ClientSocket} = gen_tcp:accept(LSocket),
    io:format(" -> client connected~n"),
    {ok,FsmPid} = qerl_stomp_fsm:start_link([]),
    gen_tcp:controlling_process(ClientSocket,self()),
    inet:setopts(ClientSocket, [{active, once}]),
    ConnMngModule:detach(),
    {noreply,{{client_socket,ClientSocket},{data,<<>>}, {fsm,FsmPid}}};
handle_cast(_Msg,State) ->
    {noreply,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_info({tcp,ClientSocket,Bin},State) ->
    io:format("TCP data: ~p~n",[Bin]),
    {{client_socket,CSocket},{data,BinData},{fsm,FsmPid}} = State,
    NewBinData = <<BinData/binary, Bin/binary>>,
    inet:setopts(ClientSocket, [{active, once}]),
    gen_tcp:send(CSocket,["CONNECTED\nsession:SOME_ID_HERE" ++ [10,10,0]]),
    case qerl_stomp_protocol:proc(NewBinData) of
        {next,Rest} -> {noreply,{{client_socket,CSocket},{data,Rest},{fsm,FsmPid}}};
        {ok} -> {noreply,{{client_socket,CSocket},{data,<<>>},{fsm,FsmPid}}}
    end;
handle_info({tcp_closed,_ClientSocket},State) -> {stop,normal,State};
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,State) ->
    {{_,_},{_,_},{fsm,FsmPid}} = State,
    qerl_stomp_fsm:stop(FsmPid),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


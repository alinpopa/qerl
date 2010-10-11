-module(qerl_conn_listener).
-behaviour(gen_server).

-compile(export_all).

-record(conn_state,{client_socket,data = <<>>,frames = [],fsm}).

start_link(ConnMngModule,LSocket) -> gen_server:start_link(?MODULE,[ConnMngModule,LSocket],[]).

stop(Pid) ->
    gen_server:cast(Pid,{close}).

init([ConnMngModule,LSocket]) ->
    process_flag(trap_exit,true),
    gen_server:cast(self(), {listen,ConnMngModule,LSocket}),
    {ok,[]}.

handle_cast({listen,ConnMngModule,LSocket}, []) ->
    {ok,ClientSocket} = gen_tcp:accept(LSocket),
    io:format(" -> client connected~n"),
    {ok,FsmPid} = qerl_stomp_fsm:start_link([self()]),
    gen_tcp:controlling_process(ClientSocket,self()),
    inet:setopts(ClientSocket, [{active, once}]),
    ConnMngModule:detach(),
    {noreply,#conn_state{client_socket = ClientSocket,data = <<>>,frames = [],fsm = FsmPid}};
handle_cast({close},State) ->
    {stop,normal,State};
handle_cast(Msg,State) ->
    {noreply,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_info({tcp,ClientSocket,Bin},State) ->
    BinData = State#conn_state.data,
    NewBinData = <<BinData/binary, Bin/binary>>,
    inet:setopts(State#conn_state.client_socket, [{active, once}]),
    gen_tcp:send(State#conn_state.client_socket,["CONNECTED\nsession:SOME_ID_HERE" ++ [10,10,0]]),
    case qerl_stomp_protocol:parse(NewBinData,State#conn_state.frames) of
        {next,NewFrames,Rest} ->
            {noreply,State#conn_state{data = Rest,frames = NewFrames}};
        {ok,AllFrames} ->
            qerl_stomp_fsm:process(State#conn_state.fsm, AllFrames),
            {noreply,State#conn_state{data = <<>>,frames = []}}
    end;
handle_info({tcp_closed,_ClientSocket},State) ->
    qerl_stomp_fsm:stop(State#conn_state.fsm),
    {stop,normal,State};
handle_info(Info,State) ->
    {noreply,State}.

terminate(_Reason,State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.

trace(Msg) -> io:format("~p: ~p~n",[?MODULE,Msg]).


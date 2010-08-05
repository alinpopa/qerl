-module(qerl_conn_listener).
-behaviour(gen_server).

-compile(export_all).

start_link(ConnMngModule,LSocket) -> gen_server:start_link(?MODULE,[ConnMngModule,LSocket],[]).

init([ConnMngModule,LSocket]) ->
    gen_server:cast(self(), {listen,ConnMngModule,LSocket}),
    {ok,[]}.

handle_cast({listen,ConnMngModule,LSocket}, []) ->
    {ok,ClientSocket} = gen_tcp:accept(LSocket),
    gen_tcp:controlling_process(ClientSocket,self()),
    inet:setopts(ClientSocket, [{active, once}]),
    ConnMngModule:detach(),
    {noreply,{{client_socket,ClientSocket},{data,<<>>}}};
handle_cast(_Msg,State) ->
    {noreply,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_info({tcp,ClientSocket,Bin},State) ->
    {{client_socket,CSocket},{data,BinData}} = State,
    NewBinData = <<BinData/binary, Bin/binary>>,
    inet:setopts(ClientSocket, [{active, once}]),
    case qerl_stomp_protocol:is_eof(NewBinData) of
        true ->
            Parsed = qerl_stomp_protocol:parse(NewBinData),
            io:format("~p~n",[Parsed]),
            {noreply,{{client_socket,CSocket},{data,<<>>}}};
        _ ->
            io:format(" -> is_eof false~n"),
            {noreply,{{client_socket,CSocket},{data,NewBinData}}}
    end;
handle_info({tcp_closed,_ClientSocket},State) -> {stop,normal,State};
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


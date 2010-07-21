-module(qerl_server).
-author('Alin Popa').
-export([start/0]).

-define(DEFAULT_READ_BUFFER, 0).
-define(DEFAULT_PORT, 5559).
-define(MAX_TCP_CONNECTIONS, 0).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, ?MAX_TCP_CONNECTIONS}]).

%% Start the server using the default port ?DEFAULT_PORT
start() ->
    {ok, Listen} = gen_tcp:listen(?DEFAULT_PORT, ?TCP_OPTIONS),
    spawn(fun() -> accept(Listen) end),
    spawn(fun() -> loop_server(Listen) end).

%% Run the server as a separate process in order to be able to send 
%% it a stop command.
loop_server(Listen) ->
    receive
        {stop} -> gen_tcp:close(Listen);
        _      -> loop_server(Listen)
    end.

%% Accept new connections for the listening socket
%% and applies processing function only if 
accept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> proc_frame(Socket, []) end),
            accept(Listen);
        _ -> ok
    end.

%% Process incomming data by using external logic
proc_frame(Socket) -> proc_frame(Socket, []).

proc_frame(Socket, DataToProc) ->
    ProcFun = fun(X,Y) -> qerl_proc:proc_frame(X, Y) end,
    EndFun = fun(X) -> qerl_protocol:is_eof(X) end,
    case gen_tcp:recv(Socket, ?DEFAULT_READ_BUFFER) of
        {ok, Data} ->
            Frame = lists:append(DataToProc, erlang:binary_to_list(Data)),
            case EndFun(Frame) of
                true ->
                    Req = ProcFun(Frame, inet:peername(Socket)),
                    case Req of
                        {disconnect, disconnect} -> gen_tcp:close(Socket);
                        {send_response, Resp}    -> gen_tcp:send(Socket,[Resp]),
                                                    proc_frame(Socket,[]);
                        {none}                   -> proc_frame(Socket,[])
                    end;
                _    -> proc_frame(Socket, Frame)
            end;
        {error, closed} -> ok
    end.


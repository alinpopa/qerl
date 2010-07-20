-module(qerl_stomp_server).
%-export([start/0, loop/1, stop/0]).
-export([start/0, proc_frame/1, proc_frame/2, stop/0]).

-define(DEFAULT_READ_BUFFER, 0).

start() ->
    qerl_socket_server:start(?MODULE, 5559, {?MODULE, proc_frame}).

stop() -> qerl_socket_server:stop(?MODULE).

%loop(Socket) ->
%    case gen_tcp:recv(Socket, 0) of
%        {ok, Data} ->
%            gen_tcp:send(Socket, Data),
%            loop(Socket);
%        {error, closed} ->
%            ok
%    end.

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


-module(qerl_gen_server).
-behaviour(gen_server).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).

-define(DEFAULT_READ_BUFFER, 0). 
-define(DEFAULT_PORT, 5559).
-define(MAX_TCP_CONNECTIONS, 0). 
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, ?MAX_TCP_CONNECTIONS}]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

new_server() -> gen_server:call(?MODULE, {new}).
stop_server(Listen) -> gen_server:call(?MODULE, {stop, Listen}).

init([]) -> {ok, ?MODULE}.

handle_call({new}, _, _) ->
    {ok, Listen} = gen_tcp:listen(?DEFAULT_PORT, ?TCP_OPTIONS),
    spawn(fun() -> accept(Listen) end),
    {reply, Listen, Listen};
handle_call({stop, Listen}, _, _) ->
    gen_tcp:close(Listen),
    {reply, Listen, Listen}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

accept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> proc_frame(Socket, []) end),
            accept(Listen);
        _ -> ok
    end.

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


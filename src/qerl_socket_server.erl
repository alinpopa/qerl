-module(qerl_socket_server).
-behaviour(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/1, stop/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
        port,
        ip=any,
        lsocket=null}).

start(Port) ->
    io:format("Starting gen_server"),
    State = #server_state{port = Port},
    case gen_server:start(?MODULE, State, []) of
        {ok,Pid} -> register(?MODULE,Pid);
        Else -> error_logger:error_msg("error starting qerl_socket_server process",[Else])
    end.

stop() -> gen_server:call(?MODULE, {stop}).

init(State  = #server_state{port=Port}) ->
    io:format(" -> init-proc: ~p~n",[length(processes())]),
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            io:format("Listen using LSocket: ~p~n",[LSocket]),
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{})  ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket})  ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    %case qerl_fsm:start_link() of
    case qerl_fsm:start() of
        {ok, Pid} ->
            io:format(" -> qerl_fsm:start(), pid ~p, socket ~p~n",[Pid,Socket]),
            gen_tcp:controlling_process(Socket,Pid),
            qerl_fsm:set_socket(Pid,Socket);
        Else -> error_logger:error_msg("error starting qerl_fsm process",[Else])
    end.

accept(State = #server_state{lsocket=LSocket}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket}]),
    %proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket}]),
    State.

% These are just here to suppress warnings.
handle_call({stop}, _Caller, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _Caller, State) -> 
    {noreply, State}.
handle_info(Msg, Library) ->
    io:format(" -> gen_server:handle_info: Msg - ~p, Library - ~p~n",[Msg,Library]),
    {noreply, Library}.
terminate(_Reason, State) ->
    gen_tcp:close(State#server_state.lsocket),
	ok.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


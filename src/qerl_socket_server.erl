-module(qerl_socket_server).
-behaviour(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/3, stop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
        port,
        loop,
        ip=any,
        lsocket=null}).

start(Name, Port, Loop) ->
    io:format("Starting gen_server"),
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

stop(Name) ->
    gen_server:call(Name, {stop}).

init(State  = #server_state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            io:format("Listen using LSocket: ~p~n",[LSocket]),
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, {LSocket, _Pid}}, State=#server_state{lsocket = LSocket})  ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}})  ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, {LSocket, self()}}),
    {ok,Pid} = qerl_fsm:start_link(),
    gen_tcp:controlling_process(Socket, Pid),
    qerl_fsm:set_socket(Socket).

% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
    io:format("accept~n"),
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

% These are just here to suppress warnings.
handle_call({stop}, _Caller, State) ->
    LSocket = State#server_state.lsocket,
    io:format("LSocket stop: ~p~n",[LSocket]),
    CloseResponse = gen_tcp:close(LSocket),
    io:format("Close response: ~p~n",[CloseResponse]),
    {reply, ok, State};
handle_call(_Msg, _Caller, State) -> 
    {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


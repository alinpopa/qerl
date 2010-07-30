-module(qerl_fsm).
-behaviour(gen_fsm).

-export([start/0, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% states
-export(['INCOMPLETE_FRAME'/2, 'END_OF_FRAME'/2, 'WAIT_FOR_SOCKET'/2]).

-import(qerl_protocol, [is_eof/1]).
-import(qerl_utils, [append/2]).

'WAIT_FOR_SOCKET'({socket_ready, Socket}, _State) ->
    io:format(" -> wait for socket: socket - ~p~n",[Socket]),
    inet:setopts(Socket, [{active, once}]),
    {next_state, 'INCOMPLETE_FRAME', create_state_data(Socket,[])}.

'INCOMPLETE_FRAME'({data, NewData}, State) ->
    {{socket,Socket},{data,IncompleteData}} = State,
    Data = append(IncompleteData, NewData),
    case is_eof(Data) of
        true ->
            info("done parsing data:~n~p~n",[Data]),
            {next_state, 'END_OF_FRAME', create_state_data(Socket,[])};
        _    ->
            info("incomplete data:~n~p~n",[Data]),
            {next_state, 'INCOMPLETE_FRAME', create_state_data(Socket,Data)}
    end.

'END_OF_FRAME'({data, NewData}, State) ->
    {{socket, Socket}, {_, _}} = State,
    {next_state, 'INCOMPLETE_FRAME', create_state_data(Socket, NewData)}.

create_state_data(Socket,Data) -> {{socket,Socket},{data,Data}}.

info(Msg) -> info(Msg,[]).
info(Msg,Params) -> error_logger:info_msg(Msg,Params).

%% gen_fsm functions
start_link() -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).
start() -> gen_fsm:start(?MODULE, [], []).

set_socket(Pid,Socket) ->
    io:format(" -> set_socket: ~p~n",[Socket]),
    inet:setopts(Socket, [{active, once}]),
    io:format(" -> send_event: ~p~n",[Socket]),
    gen_fsm:send_event(Pid, {socket_ready,Socket}).

%% gen_fsm callbacks
init([]) -> 
    io:format(" -> gen_fsm:init~n"),
    {ok, 'WAIT_FOR_SOCKET', create_state_data(none,[])}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) -> {reply, ok, StateName, State}.

handle_info({tcp, Socket, Bin}, StateName, State) ->
    io:format(" -> handle_info: socket - ~p, StateName - ~p, State - ~p, Bin - ~p~n",[Socket,StateName,State,Bin]),
    inet:setopts(Socket, [{active, once}]),
    ListData = erlang:binary_to_list(Bin),
    ?MODULE:StateName({data,ListData},State);
handle_info({tcp_closed, _Socket}, _StateName, State) -> {stop, normal, State};
handle_info(_Info, StateName, State) -> {noreply, StateName, State}.

terminate(Reason, _StateName, State) ->
    {{socket,Socket},{data,_}} = State,
    info("terminate: Reason -> ~p, State -> ~p~n",[Reason,State]),
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


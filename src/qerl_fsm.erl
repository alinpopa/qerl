-module(qerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0, set_socket/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([parsing/2]).

-import(qerl_protocol, [is_eof/1]).
-import(qerl_utils, [append/2]).

parsing({parse,Socket}, State) ->
    {{socket,_},{data,Data}} = State,
    case is_eof(Data) of
        true ->
            info("done parsing data:~n~p~n",[Data]),
            {next_state, parsing, create_state_data(Socket,[])};
        _    ->
            info("incomplete data:~n~p~n",[Data]),
            {next_state, parsing, create_state_data(Socket,Data)}
    end.

create_state_data(Socket,Data) -> {{socket,Socket},{data,Data}}.

info(Msg) -> info(Msg,[]).
info(Msg,Params) -> error_logger:info_msg(Msg,Params).

%% gen_fsm functions
start_link() -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

set_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    gen_fsm:send_event(?MODULE, {parse,Socket}).

%% gen_fsm callbacks
init([]) -> {ok, parsing, create_state_data(none,[])}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) -> {reply, ok, StateName, State}.

handle_info({tcp, Socket, Bin}, StateName, State) ->
    inet:setopts(Socket, [{active, once}]),
    {{socket,Socket},{data,IncompleteData}} = State,
    ListData = erlang:binary_to_list(Bin),
    Data = append(IncompleteData, ListData),
    ?MODULE:StateName({parse,Socket},create_state_data(Socket,Data));
handle_info({tcp_closed, _Socket}, _StateName, State) -> {stop, normal, State};
handle_info(_Info, StateName, State) -> {noreply, StateName, State}.

terminate(Reason, _StateName, State) ->
    {{socket,Socket},{data,_}} = State,
    info("terminate: Reason -> ~p, State -> ~p~n",[Reason,State]),
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


-module(qerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([set_socket/1]).
-export([init/1, parsing/2, handle_info/3, terminate/3]).

-import(qerl_protocol, [is_eof/1]).
-import(qerl_utils, [append/2]).

start_link() ->
    io:format(" -> start link~n"),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format(" -> init~n"),
    {ok, parsing, create_state_data(none,[])}.

set_socket(Socket) ->
    io:format(" -> set socket: ~p~n",[Socket]),
    gen_fsm:send_event(?MODULE, {parse,Socket}).

parsing({parse,Socket}, State) ->
    %inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    io:format(" -> parsing~n"),
    {{socket,_},{data,IncompleteData}} = State,
    io:format(" -> ready: socket = ~p, data = ~p~n",[Socket,IncompleteData]),
    case gen_tcp:recv(Socket, 0) of
        {ok, NewData} ->
            ListData = erlang:binary_to_list(NewData),
            Data = append(IncompleteData, ListData),
            case is_eof(Data) of
                true ->
                    io:format(" -> done parsing data~n"),
                    {next_state, parsing, create_state_data(Socket,[])};
                _   ->
                    io:format(" -> incomplete data~n"),
                    inet:setopts(Socket, [{active, once}]),
                    io:format(" -> next_state~n"),
                    {next_state, parsing, create_state_data(Socket,append(Data,ListData))}
            end;
        {error,closed} ->
            io:format(" -> oh no, it closed~n"),
            {stop, normal, create_state_data(none,[])}
    end.

handle_info({tcp_closed, Socket}, _StateName, StateData) ->
    {stop, normal, StateData}.

terminate(_Reason, _StateName, StateData) ->
    {{socket,Socket},{data,_}} = StateData,
    (catch gen_tcp:close(Socket)),
    ok.

create_state_data(Socket,Data) ->
    {{socket,Socket},{data,Data}}.


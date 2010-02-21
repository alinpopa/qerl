-module(qerl_tcp).
-author('Alin Popa').
-export([start/0, start/1]).

-define(DEFAULT_READ_BUFFER, 0).
-define(DEFAULT_PORT, 5559).
-define(MAX_TCP_CONNECTIONS, 0).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, ?MAX_TCP_CONNECTIONS}]).
-define(QERL_STOMP, qerl_stomp).

loop() ->
	loop(fun ?QERL_STOMP:process_data/2, fun ?QERL_STOMP:end_request/1).

loop(FProcessor, FIsEndRequest) ->
	receive
		{start,Port} ->
			io:format("Time to start now on port ~w~n",[Port]),
			listen(Port, FProcessor, FIsEndRequest);
		stop ->
			exit(self(), kill)
	end.

listen(Port, FProcessor, FIsEndRequest) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket, FProcessor, FIsEndRequest).

% Wait for incoming connections and spawn the loop when we get one.
accept(LSocket, FProcessor, FIsEndRequest) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> loop(Socket, FProcessor, FIsEndRequest, []) end),
	accept(LSocket, FProcessor, FIsEndRequest).

loop(Socket, FProcessor, FIsEndRequest, ListData) ->
	case gen_tcp:recv(Socket,?DEFAULT_READ_BUFFER) of
		{ok, Data} ->
			NewListData = lists:append(ListData, erlang:binary_to_list(Data)),
            io:format("DATA:~p~n",[NewListData]),
			case FIsEndRequest(NewListData) of
				true ->
					FProcessorRes = FProcessor(NewListData, inet:peername(Socket)),
					case FProcessorRes of
						{send_response, Resp} -> gen_tcp:send(Socket, [Resp]);
						{disconnect,_} -> gen_tcp:close(Socket);
						_ -> nothing_to_do
					end,
					loop(Socket, FProcessor, FIsEndRequest, []);
				_   ->
					loop(Socket, FProcessor, FIsEndRequest, NewListData)
			end;
		{error, closed} ->
			ok
	end.

% Starts the tcp server using default port.
start() ->
	Pid = spawn(fun loop/0),
	Pid ! {start,?DEFAULT_PORT},
	Pid.

% Starts the tcp server on a specific port.
start(Port) ->
	case Port of
		[T|_] ->
			Pid = spawn(fun loop/0),
			Pid ! {start,list_to_integer(T)},
			Pid;
		_ -> io:format("No valid port passed~n",[])
	end.


-module(qerl_tcp).
-author('Alin Popa').
-export([start/0, start/1]).

-define(DEFAULT_READ_BUFFER, 0).
-define(DEFAULT_PORT, 5559).
-define(MAX_TCP_CONNECTIONS, 1000).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, ?MAX_TCP_CONNECTIONS}]).
-define(QERL_STOMP, qerl_stomp).

loop() ->
	loop(fun ?QERL_STOMP:process_data/2).

loop(F) ->
	receive
		{start,Port} ->
			io:format("Time to start now on port ~w~n",[Port]),
			listen(Port, F);
		stop ->
			exit(self(), kill)
	end.

% Call echo:listen(Port) to start the service.
listen(Port, F) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket, F).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket, F) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> loop(Socket, F, []) end),
	accept(LSocket, F).

% Echo back whatever data we receive on Socket.
loop(Socket, F, RawData) ->
	case gen_tcp:recv(Socket,?DEFAULT_READ_BUFFER) of
		{ok, Data} ->
			case ?QERL_STOMP:end_request(Data) of
				true ->
					io:format("Frame start: ~s~n", [?QERL_STOMP:process_header_start(Data)]),
					loop(Socket, F, RawData);
				_   ->
					loop(Socket, F, lists:append(RawData, erlang:binary_to_list(Data)))
			end;
		{error, closed} ->
			ok
	end.

start() ->
	Pid = spawn(fun loop/0),
	Pid ! {start,?DEFAULT_PORT},
	Pid.

start(Port) ->
	case Port of
		[T|_] ->
			Pid = spawn(fun loop/0),
			Pid ! {start,list_to_integer(T)},
			Pid;
		_ -> io:format("No valid port passed~n",[])
	end.


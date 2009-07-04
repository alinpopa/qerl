-module(qerl_stomp).
-export([process_data/2, send_response/1]).
-export([end_request/1, connect/2, process_header_start/1]).
-export([strip_carriage_return/1]).

connect(Data, ClientInfo) ->
	erlang:list_to_binary("CONNECTED" ++ "\n" ++ "session: " ++
							generate_session_id(ClientInfo) ++
							[10,10,0]).

parse_header(ReqData) ->
	ListReqData = strip_carriage_return(erlang:binary_to_list(ReqData)),
	AtomHeader = erlang:list_to_atom(string:to_lower(string:substr(ListReqData, 1, string:chr(ListReqData, 10) - 1))),
	AtomHeader.

process_data(Data, ClientInfo) ->
	Header = parse_header(Data),
	case Header of
		connect  -> {ok, connect(Data, ClientInfo)};
		_ 		 -> {error, Header}
	end.

generate_session_id(ClientInfo) ->
	{Ok,{Ip,Port}} = ClientInfo,
	SessionId = lists:flatten(io_lib:format("ok:~w-ip:~w-port:~w",[Ok, Ip, Port])),
	SessionId.

send_response(Data) ->
	ListReqData = strip_carriage_return(erlang:binary_to_list(Data)),
	io:format("Got list: ~w~n",[ListReqData]),
	NewList = lists:takewhile(fun(X) -> X =/= 0 end, ListReqData),
	io:format("~w~n",[NewList]),
	erlang:list_to_atom(string:to_lower(string:substr(ListReqData, string:len(ListReqData), 1))).

end_request(ListData) ->
	ListReqData = strip_carriage_return(ListData),
	case null_expected(ListReqData) of
		true -> lists:member(0, ListReqData);
		_ -> false
	end.

null_expected(ListData) ->
	T1 = string:str(ListData, [10,10]) =/= 0,
	if
		T1 == true -> true;
		true ->
			T2 = string:str(ListData, [10,0,10,0]) =/= 0,
			if
				T2 == true -> true;
				true -> false
			end
	end.

strip_carriage_return(List) ->
	lists:filter(fun(X) -> X =/= 13 end, List).

get_frame_start(Data) ->
	ListReqData = strip_carriage_return(erlang:binary_to_list(Data)),
	string:substr(ListReqData, 1, string:chr(ListReqData, 10) - 1).

process_header_start(Data) ->
	FrameStart = get_frame_start(Data),
	io:format("F: ~s~n",[FrameStart]),
	case FrameStart of
		"CONNECT"     -> "SERVER: Connect";
		"SEND"        -> "SERVER: Send";
		"SUBSCRIBE"   -> "SERVER: Subscribe";
		"UNSUBSCRIBE" -> "SERVER: Unsubscribe";
		"BEGIN"       -> "SERVER: Begin";
		"COMMIT"      -> "SERVER: Commit";
		"ABORT"       -> "SERVER: Abort";
		"ACK"         -> "SERVER: Ack";
		"DISCONNECT"  -> "SERVER: Disconnect";
		_             -> "SERVER: Unknown header"
	end.


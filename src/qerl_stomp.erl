-module(qerl_stomp).
-export([process_data/2, end_request/1]).

% Process data got from client (passed as a List). Also ClientInfo is passed
% in order to generate a session id.
process_data(RawListData, ClientInfo) ->
	ListData = strip_carriage_return(RawListData),
	io:format("----------REQUEST---------~n",[]),
	io:format("ListData: ~w~nClientInfo: ~w~n",[ListData, ClientInfo]),
	io:format("----------REQUEST---------~n",[]),
	Header = get_header(ListData),
	case Header of
		"DISCONNECT" ->
			{disconnect, disconnect};
		"CONNECT" ->
			{send_response, response_connected(ClientInfo)};
		_ ->
			{send_response, response_error("Unknown STOMP action: " ++ Header)}
	end.

% Generate a session id based on ClientInfo.
generate_session_id(ClientInfo) ->
	{Ok,{Ip,Port}} = ClientInfo,
	lists:flatten(io_lib:format("ok:~w-ip:~w-port:~w",[Ok, Ip, Port])).

% Create the STOMP connected response.
response_connected(ClientInfo) ->
	format_response("CONNECTED" ++
		"\n" ++
		"session" ++
		generate_session_id(ClientInfo) ++
		[10,10,0]).

% Create the STOMP error response by adding an extra Error Message to it.
response_error(ErrorMessage) ->
	format_response("ERROR" ++
		"\n" ++
		"message:" ++
		ErrorMessage ++
		[10,10,0]).
% Extra format the response before sending it back to client.
format_response(ListResponse) ->
	io:format("------------START RESPONSE-------------~n",[]),
	io:format("STRING:~n~s~n",[ListResponse]),
	io:format("RAW:~n~w~n",[ListResponse]),
	io:format("-------------END RESPONSE--------------~n",[]),
	ListResponse.

% Check if the passed ListData contains the null byte as the last element.
end_request(ListData) ->
	ListReqData = strip_carriage_return(ListData),
	case null_expected(ListReqData) of
		true ->
			Last = lists:last(ListReqData) =:= 10,
			if
				Last == true ->
					TrimListData = string:substr(ListReqData, 1, erlang:length(ListReqData)-1),
					io:format("Trim data~n",[]),
					lists:last(TrimListData) =:= 0;
				true -> false
			end;
		_ -> false
	end.

% Check if in the passed ListData, a null byte should be expected.
null_expected(ListData) ->
	TrimData = string:substr(ListData, 1, erlang:length(ListData)-1),
	(string:str(TrimData, [10,10]) =/= 0
	orelse string:str(TrimData, [10,0,10,0]) =/= 0).

% Filter all occurences of carriage return (ASCII 13) in the passed List.
strip_carriage_return(List) ->
	lists:filter(fun(X) -> X =/= 13 end, List).

% Return header from a passed ListData.
% The return header will be a character list.
get_header(ListData) ->
	string:substr(ListData, 1, string:chr(ListData, 10) - 1).


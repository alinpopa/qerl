-module(qerl_proc).
-export([proc_frame/2]).

%%----------------------------------------------------------------------
%% @spec (Frame, ClientInfo) -> {disconnect, disconnect}   |
%%                              {send_response, Response}  |
%%                              {none}
%
%% @end
%%----------------------------------------------------------------------
proc_frame(Frame, ClientInfo) ->
    ParseResp = {{command, Command},{headers,Headers},{body,Body}} = qerl_protocol:parse_frame(Frame),
    io:format("Req: ~p~n",[ParseResp]),
    {{command, Command},{headers,Headers},{body,Body}} = ParseResp,
	case Command of
		"DISCONNECT" -> {disconnect, disconnect};
		"CONNECT" -> {send_response, response_connected(ClientInfo)};
        "SUBSCRIBE" -> {send_response, proc_subscribe()};
		_ -> {send_response, response_error("Unknown STOMP action: " ++ Command)}
	end.

%%
%% Intenal functions
%%
response_connected(ClientInfo) ->
	format_response([10] ++
        "CONNECTED" ++
		[10] ++
		"session:" ++
		generate_session_id(ClientInfo) ++
		[10,10,0]).

response_error(ErrorMessage) ->
	format_response([10] ++
        "ERROR" ++
		[10] ++
		"message:" ++
		ErrorMessage ++
		[10,10,0]).

response_message(FrameBody) ->
    format_response([10] ++
        "MESSAGE" ++
        [10] ++
        "destination:/queue/some_queue" ++
        [10] ++
        "message-id: some-UUID" ++
        [10] ++
        "content-length: " ++ integer_to_list(length(FrameBody)) ++
        [10,10] ++
        FrameBody++ [0]).

format_response(Response) -> Response.

generate_session_id(ClientInfo) ->
	{Ok,{Ip,Port}} = ClientInfo,
	lists:flatten(io_lib:format("ok:~w-ip:~w-port:~w",[Ok, Ip, Port])).

proc_subscribe() ->
    Resp = response_message("Some queue message here"),
    io:format("RESPONSE: ~s~n",[Resp]),
    Resp.


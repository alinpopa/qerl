-module(qerl_stomp_protocol).
-export([is_eof/1, parse/1]).

-import(binary, [bin_to_list/1]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).
-define(COLLON,58).

is_eof(Bin) ->
    case binary:match(drop_cr(Bin),<<?LF,?NULL>>) of
        nomatch -> false;
        _ -> true 
    end.

parse(Bin) ->
    LfBin = drop_cr(get_valid_data(Bin)),
    parse_msg(binary:split(LfBin,<<?LF>>,[trim])). 

parse_msg([<<"CONNECT">>|T]) -> {connect, {headers,parse_headers(T)}};
parse_msg([<<"SEND">>|T]) -> {send,{headers,parse_headers(T)},{body,parse_body(T)}};
parse_msg([<<"SUBSCRIBE">>|T]) -> {subscribe,{headers,parse_headers(T)}};
parse_msg([<<"UNSUBSCRIBE">>|T]) -> {unsubscribe,{headers,parse_headers(T)}};
parse_msg([<<"BEGIN">>|T]) -> {begin_tx,{headers,parse_headers(T)}};
parse_msg([<<"COMMIT">>|T]) -> {commit,{headers,parse_headers(T)}};
parse_msg([<<"ABORT">>|T]) -> {abort,{headers,parse_headers(T)}};
parse_msg([<<"ACK">>|T]) -> {ack,{headers,parse_headers(T)}};
parse_msg([<<"DISCONNECT">>|T]) -> {disconnect,{headers,parse_headers(T)}};
parse_msg(Bin) -> {something_else,Bin}.

drop_cr(Bin) when is_binary(Bin) -> binary:replace(Bin,<<?CR>>,<<>>,[global]);
drop_cr(_Bin) -> <<>>.

parse_headers([]) -> [];
parse_headers([H|_]) ->
    BinList = binary:split(H,<<?LF>>,[global]),
    parse_headers(BinList,[]).

parse_headers([],Headers) -> to_headers(Headers);
parse_headers([H|T],Headers) ->
    case H of
        <<>> -> to_headers(Headers);
        <<?NULL>> -> to_headers(Headers);
        Else -> parse_headers(T,[Else|Headers])
    end.

parse_body([]) -> [];
parse_body([H|_]) ->
    [_H|Body] = binary:split(H,<<?LF,?LF>>,[global]),
    [BinBody|_] = Body,
    BinList = binary:split(BinBody,<<?LF>>,[global]),
    parse_body(BinList,[]).

parse_body([],Body) -> to_list(Body);
parse_body([H|T],Body) ->
    case H of
        <<>> -> to_list(Body);
        <<?NULL>> -> to_list(Body);
        Else -> parse_body(T,[Else|Body])
    end.

get_valid_data(Bin) ->
    [H|_] = binary:split(Bin,<<?NULL,?LF>>,[trim]),
    H.

to_list(BinList) -> lists:map(fun(X) -> binary:bin_to_list(X) end, lists:reverse(BinList)).

to_headers(BinHeaders) -> to_headers(BinHeaders,[]).

to_headers([],Headers) -> Headers;
to_headers([H|T],Headers) ->
    {BinK,BinV} = list_to_tuple(binary:split(H,<<":">>)),
    to_headers(T,[{bin_to_list(BinK), trim_left(bin_to_list(BinV))}|Headers]).

trim_left([32|T]) -> trim_left(T);
trim_left(L) -> L.


-module(qerl_stomp_protocol).
-export([is_eof/1, parse/1]).

-import(binary, [bin_to_list/1, match/2, replace/4, split/2, split/3]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).
-define(COLLON,58).

is_eof(Bin) ->
    case match(drop_cr(Bin),<<?LF,?NULL>>) of
        nomatch -> false;
        _ -> true 
    end.

parse(Bin) ->
    LfBin = drop_cr(get_valid_data(Bin)),
    parse_msg(split(LfBin,<<?LF>>,[trim])). 

parse_msg([<<"CONNECT">>,T]) -> {connect, parse_frame(T)};
parse_msg([<<"SEND">>,T]) -> {send,parse_frame(T)};
parse_msg([<<"SUBSCRIBE">>,T]) -> {subscribe,{headers,parse_headers(T)}};
parse_msg([<<"UNSUBSCRIBE">>,T]) -> {unsubscribe,{headers,parse_headers(T)}};
parse_msg([<<"BEGIN">>,T]) -> {begin_tx,{headers,parse_headers(T)}};
parse_msg([<<"COMMIT">>,T]) -> {commit,{headers,parse_headers(T)}};
parse_msg([<<"ABORT">>,T]) -> {abort,{headers,parse_headers(T)}};
parse_msg([<<"ACK">>,T]) -> {ack,{headers,parse_headers(T)}};
parse_msg([<<"DISCONNECT">>,T]) -> {disconnect,{headers,parse_headers(T)}};
parse_msg(Bin) -> {something_else,Bin}.

parse_frame(Frame) ->
    case binary:first(Frame) of
        ?LF -> {{headers,[]},{body,parse_body(Frame)}};
        _Else -> parse_frame(split(Frame,<<?LF>>,[trim]), [])
    end.

parse_body(<<?LF,Rest/binary>>) -> to_list_body(Rest);
parse_body(BinData) -> to_list_body(BinData).

parse_frame([Rest],Acc) -> {{headers,parse_headers(Acc)},{body,parse_body(Rest)}};
parse_frame([<<>>,Rest], Acc) -> {{headers,parse_headers(Acc)},{body,parse_body(Rest)}};
parse_frame([Head,Rest], Acc) -> parse_frame(split(Rest,<<?LF>>,[trim]), [Head|Acc]).

to_list_body(BinBody) ->
    ListBody = binary:bin_to_list(BinBody),
    lists:takewhile(fun(X) -> X =/= 0 end,ListBody).

drop_cr(Bin) when is_binary(Bin) -> replace(Bin,<<?CR>>,<<>>,[global]);
drop_cr(_Bin) -> <<>>.

parse_headers([]) -> [];
parse_headers(Headers) -> parse_headers(Headers,[]).

parse_headers([],Headers) -> to_headers(Headers);
parse_headers([H|T],Headers) ->
    case H of
        <<>> -> to_headers(Headers);
        <<?NULL>> -> to_headers(Headers);
        Else -> parse_headers(T,[Else|Headers])
    end.

%parse_body([]) -> [];
%parse_body([H|_]) ->
%    [_H|Body] = split(H,<<?LF,?LF>>,[global]),
%    [BinBody|_] = Body,
%    BinList = split(BinBody,<<?LF>>,[global]),
%    parse_body(BinList,[]).
%
%parse_body([],Body) -> to_list(Body);
%parse_body([H|T],Body) ->
%    case H of
%        <<>> -> to_list(Body);
%        <<?NULL>> -> to_list(Body);
%        Else -> parse_body(T,[Else|Body])
%    end.

get_valid_data(Bin) ->
    [H|_] = split(Bin,<<?NULL,?LF>>,[trim]),
    H.

to_list(BinList) -> lists:map(fun(X) -> bin_to_list(X) end, lists:reverse(BinList)).

to_headers(BinHeaders) -> to_headers(BinHeaders,[]).

to_headers([],Headers) -> lists:reverse(Headers);
to_headers([H|T],Headers) ->
    {BinK,BinV} = list_to_tuple(create_header(split(H,<<":">>))),
    to_headers(T,[{bin_to_list(BinK), trim_left(bin_to_list(BinV))}|Headers]).

create_header(L) ->
    case length(L) of
        0 -> [<<>>,<<>>];
        1 ->
            [H|_] = L,
            [H,<<>>];
        _ -> lists:sublist(L,2)
    end.

trim_left([32|T]) -> trim_left(T);
trim_left(L) -> L.


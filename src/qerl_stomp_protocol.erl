-module(qerl_stomp_protocol).
-export([is_eof/1, parse/1, proc/1]).

-import(binary, [bin_to_list/1, match/2, replace/4, split/2, split/3]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).
-define(COLLON,58).

is_eof(Bin) ->
    case match(Bin,<<?NULL>>) of
        nomatch -> false;
        _ -> true 
    end.

proc(Bin) ->
    case split(Bin,<<?NULL>>) of
        [Rest] -> {next,Rest};
        [Frame,Rest] ->
            proc_frame(Frame),
            case Rest of
                <<>> -> {ok};
                <<?CR,?LF>> -> {ok};
                <<?LF>> -> {ok};
                Else ->
                    io:format("Rest: ~p~n",[Else]),
                    proc(Rest)
            end
    end.

proc_frame(Frame) ->
    io:format("Proccess frame: ~p~n",[Frame]).

parse(Frame) ->
    LfBin = drop_cr(get_valid_data(Frame)),
    parse_msg(split(LfBin,<<?LF>>,[trim])). 

parse_msg([<<"CONNECT">>,Frame]) -> {connect,{headers,get_headers(Frame)}};
parse_msg([<<"SEND">>,Frame]) -> {send,{headers,get_headers(Frame)},{body,get_body(Frame)}};
parse_msg([<<"SUBSCRIBE">>,Frame]) -> {subscribe,{headers,get_headers(Frame)}};
parse_msg([<<"UNSUBSCRIBE">>,Frame]) -> {unsubscribe,{headers,get_headers(Frame)}};
parse_msg([<<"BEGIN">>,Frame]) -> {begin_tx,{headers,get_headers(Frame)}};
parse_msg([<<"COMMIT">>,Frame]) -> {commit,{headers,get_headers(Frame)}};
parse_msg([<<"ABORT">>,Frame]) -> {abort,{headers,get_headers(Frame)}};
parse_msg([<<"ACK">>,Frame]) -> {ack,{headers,get_headers(Frame)}};
parse_msg([<<"DISCONNECT">>,Frame]) -> {disconnect,{headers,get_headers(Frame)}};
parse_msg(Bin) -> {something_else,Bin}.


get_body(Frame) ->
    case has_headers(Frame) of
        true -> parse_body_with_headers(Frame);
        false -> parse_body_without_headers(Frame)
    end.

get_headers(Frame) ->
    case has_headers(Frame) of
        true -> parse_headers(Frame);
        false -> []
    end.

parse_headers(Frame) -> parse_headers(split(Frame,<<?LF>>,[trim]),[]).

parse_headers([],Headers) -> to_headers(Headers);
parse_headers([Head],Headers) -> to_headers(Headers);
parse_headers([<<>>,_Rest],Headers) -> to_headers(Headers);
parse_headers([H,Rest],Headers) -> parse_headers(split(Rest,<<?LF>>,[trim]),[H|Headers]).

has_headers(Frame) ->
    case binary:first(Frame) of
        ?LF -> false;
        _Else -> true
    end.

parse_body_with_headers([_Headers,Rest]) -> to_list_body(Rest);
parse_body_with_headers([_H|_T]) -> [];
parse_body_with_headers([]) -> [];
parse_body_with_headers(BinData) -> parse_body_with_headers(split(BinData,<<?LF,?LF>>,[trim])).

parse_body_without_headers(<<?LF,Rest/binary>>) -> to_list_body(Rest);
parse_body_without_headers(BinData) -> to_list_body(BinData).

to_list_body(BinBody) ->
    ListBody = binary:bin_to_list(BinBody),
    lists:takewhile(fun(X) -> X =/= 0 end,ListBody).

drop_cr(Bin) when is_binary(Bin) -> replace(Bin,<<?CR>>,<<>>,[global]);
drop_cr(_Bin) -> <<>>.

get_valid_data(Bin) ->
    [H|_] = split(Bin,<<?NULL,?LF>>,[]),
    H.

to_list(BinList) -> lists:map(fun(X) -> bin_to_list(X) end, lists:reverse(BinList)).

to_headers(BinHeaders) -> to_headers(BinHeaders,[]).

to_headers([],Headers) -> Headers;
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


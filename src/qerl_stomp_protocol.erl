-module(qerl_stomp_protocol).
-export([is_eof/1, parse/1, parse/1,drop/2]).

-import(binary, [bin_to_list/1, match/2, replace/4, split/2, split/3]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).

is_eof(BinData) ->
    LfBin = drop_invalid_beginning_frame(drop(cr,BinData)),
    case match(LfBin,<<?NULL>>) of
        nomatch -> false;
        _ ->
            case match(LfBin,<<?LF,?LF>>) of
                nomatch -> false;
                _ -> true
            end
    end.

parse(Frame) ->
    LfBin = drop_invalid_beginning_frame(drop(cr, data_without_null_and_lf(Frame))),
    trace(LfBin),
    ToParse = split(LfBin,<<?LF>>,[]),
    case length(ToParse) of
        1 ->
            [H] = ToParse,
            parse_msg([H,<<>>]);
        _ -> parse_msg(ToParse)
    end.

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

parse_headers(Frame) -> parse_headers(split(Frame,<<?LF>>,[]),[]).

parse_headers([],Headers) -> to_headers(Headers);
parse_headers([Head],Headers) -> to_headers([Head|Headers]);
parse_headers([<<>>,_Rest],Headers) -> to_headers(Headers);
parse_headers([H,Rest],Headers) -> parse_headers(split(Rest,<<?LF>>,[]),[H|Headers]).

has_headers(<<>>) -> false;
has_headers(Frame) ->
    case binary:first(Frame) of
        ?LF -> false;
        _Else -> true
    end.

parse_body_with_headers([_Headers,Rest]) -> to_list_body(Rest);
parse_body_with_headers([_H|_T]) -> [];
parse_body_with_headers([]) -> [];
parse_body_with_headers(BinData) -> parse_body_with_headers(split(BinData,<<?LF,?LF>>,[])).

parse_body_without_headers(<<?LF,Rest/binary>>) -> to_list_body(Rest);
parse_body_without_headers(BinData) -> to_list_body(BinData).

to_list_body(BinBody) ->
    ListBody = binary:bin_to_list(BinBody),
    lists:takewhile(fun(X) -> X =/= 0 end,ListBody).

drop(What,Bin) when is_binary(Bin) ->
    case What of
        cr -> replace(Bin,<<?CR>>,<<>>,[global]);
        null -> replace(Bin,<<?NULL>>,<<>>,[global]);
        _ -> Bin
    end;
drop(_What,_Bin) -> <<>>.

data_without_null_and_lf(BinData) ->
    [H|_] = split(BinData,<<?NULL,?LF>>,[]),
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

drop_invalid_beginning_frame(<<>>) -> <<>>;
drop_invalid_beginning_frame(<<?LF,Rest/binary>>) ->
    drop_invalid_beginning_frame(Rest);
drop_invalid_beginning_frame(Frame) -> Frame.

trace(Msg) -> io:format("~p: ~p~n",[?MODULE,Msg]).


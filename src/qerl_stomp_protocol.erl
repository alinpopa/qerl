-module(qerl_stomp_protocol).
-export([is_eof/1, parse/1]).
-define(LF, 10).
-define(NULL, 0).

%%
%% @doc Check if the passedin binary contains
%% an end of frame character (NULL).
%%
-spec is_eof(BinData :: binary()) -> true | false.
is_eof(BinData) ->
    Parsers = [fun(ParsedData) ->
                    qerl_stomp_utils:drop(cr, ParsedData)
               end,
               fun drop_invalid_beginning_frame/1],
    ToParse = lists:foldl(fun(F, Acc) -> F(Acc) end, BinData, Parsers),
    case binary:match(ToParse, <<(?NULL)>>) of
      nomatch -> false;
      _ -> binary:match(ToParse, <<(?LF), (?LF)>>) =/= nomatch
    end.

%%
%% @doc Parse the incoming binary Frame
%% and returns domain specific data.
%%
-spec parse(Frame :: binary()) -> tuple().
parse(Frame) ->
    Parsers = [fun data_without_null_and_lf/1,
               fun(DataWithoutNullAndLf) ->
                    qerl_stomp_utils:drop(cr, DataWithoutNullAndLf)
               end,
               fun drop_invalid_beginning_frame/1,
               fun(ParsedFrame) ->
                    binary:split(ParsedFrame, <<(?LF)>>, [])
               end],
    ToParse = lists:foldl(fun(F, Acc) -> F(Acc) end, Frame, Parsers),
    case ToParse of
      [H] -> parse_msg([H, <<>>]);
      _ -> parse_msg(ToParse)
    end.

%% @private
parse_msg([<<"CONNECT">>, Frame]) ->
    {connect, {headers, get_headers(Frame)}};
parse_msg([<<"SEND">>, Frame]) ->
    {send, {headers, get_headers(Frame)},
     {body, get_body(Frame)}};
parse_msg([<<"SUBSCRIBE">>, Frame]) ->
    {subscribe, {headers, get_headers(Frame)}};
parse_msg([<<"UNSUBSCRIBE">>, Frame]) ->
    {unsubscribe, {headers, get_headers(Frame)}};
parse_msg([<<"BEGIN">>, Frame]) ->
    {begin_tx, {headers, get_headers(Frame)}};
parse_msg([<<"COMMIT">>, Frame]) ->
    {commit, {headers, get_headers(Frame)}};
parse_msg([<<"ABORT">>, Frame]) ->
    {abort, {headers, get_headers(Frame)}};
parse_msg([<<"ACK">>, Frame]) ->
    {ack, {headers, get_headers(Frame)}};
parse_msg([<<"DISCONNECT">>, Frame]) ->
    {disconnect, {headers, get_headers(Frame)}};
%% Debug/helper commands, that are not part of STOMP specs.
parse_msg([<<"QINFO">>, _]) -> {queue_info};
parse_msg([UnknownBinCommand, _]) ->
    UnknownCommand =
	erlang:binary_to_list(UnknownBinCommand),
    {unknown_command, UnknownCommand}.

%% @private
get_body(Frame) ->
    case has_headers(Frame) of
      true -> parse_body_with_headers(Frame);
      false -> parse_body_without_headers(Frame)
    end.

%% @private
get_headers(Frame) ->
    case has_headers(Frame) of
      true -> parse_headers(Frame);
      false -> []
    end.

%% @private
parse_headers(Frame) ->
    parse_headers(binary:split(Frame, <<(?LF)>>, []), []).

%% @private
parse_headers([], Headers) -> to_headers(Headers);
parse_headers([Head], Headers) ->
    to_headers([Head | Headers]);
parse_headers([<<>>, _Rest], Headers) ->
    to_headers(Headers);
parse_headers([H, Rest], Headers) ->
    parse_headers(binary:split(Rest, <<(?LF)>>, []),
		  [H | Headers]).

%% @private
has_headers(<<>>) -> false;
has_headers(Frame) ->
    case binary:first(Frame) of
      ?LF -> false;
      _Else -> true
    end.

%% @private
parse_body_with_headers([_Headers, Rest]) ->
    to_list_body(Rest);
parse_body_with_headers([_H | _T]) -> [];
parse_body_with_headers([]) -> [];
parse_body_with_headers(BinData) ->
    parse_body_with_headers(binary:split(BinData,
					 <<(?LF), (?LF)>>, [])).

%% @private
parse_body_without_headers(<<(?LF), Rest/binary>>) ->
    to_list_body(Rest);
parse_body_without_headers(BinData) ->
    to_list_body(BinData).

%% @private
to_list_body(BinBody) ->
    ListBody = binary:bin_to_list(BinBody),
    lists:takewhile(fun (X) -> X =/= 0 end, ListBody).

%% @private
data_without_null_and_lf(BinData) ->
    [H | _] = binary:split(BinData, <<(?NULL), (?LF)>>, []),
    H.

%% @private
to_headers(BinHeaders) -> to_headers(BinHeaders, []).

%% @private
to_headers([], Headers) -> Headers;
to_headers([H | T], Headers) ->
    {BinK, BinV} =
	list_to_tuple(create_header(binary:split(H, <<":">>))),
    to_headers(T,
	       [{binary:bin_to_list(BinK),
		 trim_left(binary:bin_to_list(BinV))}
		| Headers]).

%% @private
create_header(L) ->
    case length(L) of
      0 -> [<<>>, <<>>];
      1 -> [H | _] = L, [H, <<>>];
      _ -> lists:sublist(L, 2)
    end.

%% @private
trim_left([32 | T]) -> trim_left(T);
trim_left(L) -> L.

%% @private
drop_invalid_beginning_frame(<<>>) -> <<>>;
drop_invalid_beginning_frame(<<(?LF), Rest/binary>>) ->
    drop_invalid_beginning_frame(Rest);
drop_invalid_beginning_frame(Frame) -> Frame.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_eof_for_unary_bin_element_test() ->
  false = is_eof(<<0>>).

is_eof_for_multiple_bin_elements_test() ->
  true = is_eof(<<1,10,10,0,1>>).

is_eof_should_return_false_for_empty_binary_test() ->
  false = is_eof(<<>>).

is_eof_should_return_false_when_no_eof_bit_test() ->
  false = is_eof(<<1,2,3>>).

should_parse_connect_frame_test() ->
  Result = parse(<<"CONNECT\nhead1:value1\nhead2:value2">>),
  {connect,{headers,[{Head1,Value1},{Head2,Value2}]}} = Result,
  Head1 = "head1",
  Value1 = "value1",
  Head2 = "head2",
  Value2 = "value2".

should_parse_send_frame_test() ->
  Result = parse(<<"SEND\nuser:pass\n\ntest body">>),
  {send,{headers,[{User,Password}]},{body,Body}} = Result,
  User = "user",
  Password = "pass",
  Body = "test body".

-endif.


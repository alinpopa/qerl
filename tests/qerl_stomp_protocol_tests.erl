-module(qerl_stomp_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TO_TEST, qerl_stomp_protocol).

is_eof_for_unary_bin_element_test() ->
  false = ?TO_TEST:is_eof(<<0>>).

is_eof_for_multiple_bin_elements_test() ->
  true = ?TO_TEST:is_eof(<<1,10,10,0,1>>).

is_eof_should_return_false_for_empty_binary_test() ->
  false = ?TO_TEST:is_eof(<<>>).

is_eof_should_return_false_when_no_eof_bit_test() ->
  false = ?TO_TEST:is_eof(<<1,2,3>>).

should_parse_connect_frame_test() ->
  Result = ?TO_TEST:parse(<<"CONNECT\nhead1:value1\nhead2:value2">>),
  {connect,{headers,[{Head1,Value1},{Head2,Value2}]}} = Result,
  Head1 = "head1",
  Value1 = "value1",
  Head2 = "head2",
  Value2 = "value2".

should_parse_send_frame_test() ->
  Result = ?TO_TEST:parse(<<"SEND\nuser:pass\n\ntest body">>),
  {send,{headers,[{User,Password}]},{body,Body}} = Result,
  User = "user",
  Password = "pass",
  Body = "test body".


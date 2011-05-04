-module(qerl_stomp_utils_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TO_TEST, qerl_stomp_utils).

empty_binary_is_returned_when_passed_in_binary_is_empty_test() ->
  <<>> = ?TO_TEST:drop(anything,<<>>).

should_remove_CR_from_binary_test() ->
  <<10,10,0,1,2,3>> = ?TO_TEST:drop(cr,<<10,13,10,0,1,2,3>>).

should_remove_LF_from_binary_test() ->
  <<13,0,1,2>> = ?TO_TEST:drop(lf,<<10,13,10,0,1,2>>).

should_remove_NULL_from_binary_test() ->
  <<10,10,13>> = ?TO_TEST:drop(null,<<10,0,10,0,13,0>>).


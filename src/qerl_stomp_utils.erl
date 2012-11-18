-module(qerl_stomp_utils).
-export([drop/2]).

-import(binary, [replace/4]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).

%% Drop specific byte from the given binary
drop(_,<<>>) -> <<>>;
drop(What,Bin) when is_binary(Bin) ->
    case What of
        cr -> replace(Bin,<<?CR>>,<<>>,[global]);
        lf -> replace(Bin,<<?LF>>,<<>>,[global]);
        null -> replace(Bin,<<?NULL>>,<<>>,[global]);
        _ -> Bin
    end;
drop(_What,_Bin) -> <<>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_binary_is_returned_when_passed_in_binary_is_empty_test() ->
    ?assertEqual(<<>>, drop(anything,<<>>)).

should_remove_CR_from_binary_test() ->
    ?assertEqual(<<10,10,0,1,2,3>>, drop(cr,<<10,13,10,0,1,2,3>>)).

should_remove_LF_from_binary_test() ->
    ?assertEqual(<<13,0,1,2>>, drop(lf,<<10,13,10,0,1,2>>)).

should_remove_NULL_from_binary_test() ->
    ?assertEqual(<<10,10,13>>, drop(null,<<10,0,10,0,13,0>>)).

-endif.


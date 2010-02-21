-module(qerl_utils).
-export([append/2, strip_cr/1, trim/1, bin_to_list/1]).
-import(lists, [filter/2, last/1, sublist/2, map/2]).

append([], List) -> List;
append([First|Rest], List) -> [First|append(Rest,List)].

%% Strip all carriage return characters - 13
strip_cr(List) ->
	filter(fun(X) -> X =/= 13 end, List).

trim([]) -> [];
trim(List) ->
    LTrim = trimL(List),
    trimR(LTrim).
    
trimL([]) -> [];
trimL([H|T]) ->
    case H of
        10 -> T;
        _ -> [H|T]
    end.

trimR([]) -> [];
trimR(List) ->
    Length = length(List),
    case last(List) of
        10 -> trimR(sublist(List, Length-1));
        0  -> trimR(sublist(List, Length-1));
        _ -> List
    end.

%% Transform a list of binary elements to list of lists.
bin_to_list([]) -> [];
bin_to_list(List) ->
    map(fun(X) -> trim(binary_to_list(X)) end, List).


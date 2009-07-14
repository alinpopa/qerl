-module(qerl_utils).
-export([append/2]).

append([], List) -> List;
append([First|Rest], List) -> [First|append(Rest,List)].


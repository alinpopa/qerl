-module(qerl_queue).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,0},
     {produce,2},
     {consume,1}];
behaviour_info(_Other) ->
    undefined.


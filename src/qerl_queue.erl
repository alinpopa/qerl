-module(qerl_queue).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{produce,2},
     {consume,0},
     {info,0}
    ];
behaviour_info(_Other) ->
    undefined.


-module(qerl_memory_queue).
-behaviour(qerl_queue).

-export([init/0,produce/1,consume/0,stop/0]).

init() ->
    Queue = queue:new(),
    register(?MODULE,spawn(fun() -> loop(Queue) end)).

stop() ->
    Pid = whereis(?MODULE),
    exit(Pid,kill).

loop(Q) ->
    receive
        {Producer,produce,Msg} ->
            NewQ = queue:in(Msg,Q),
            Producer ! {produce,ok},
            loop(NewQ);
        {Consumer,consume} ->
            case queue:out(Q) of
                {{value,Msg},NewQ} ->
                    Consumer ! {consume,Msg},
                    loop(NewQ);
                {empty,_} ->
                    Consumer ! {err, "no message"},
                    loop(Q)
            end;
        _Else -> loop(Q)
    end.

produce(Msg) ->
    ?MODULE ! {self(),produce,Msg},
    receive
        {produce,ok} -> io:format("Message produced~n");
        Else -> io:format("ERROR producing message: ~p~n",[Else])
    end.

consume() ->
    ?MODULE ! {self(),consume},
    receive
        {consume,Msg} -> io:format("Got msg from queue: ~p~n",[Msg]);
        {err,Msg} -> io:format("Queue is empty, no message to consume: ~p~n",[Msg]);
        Else ->
            io:format("ERROR consuming message: ~p~n",[Else])
    end.


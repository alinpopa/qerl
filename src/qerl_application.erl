-module(qerl_application).
-behaviour(application).
-export([start/2,stop/1,go/0]).

start(_Type, StartArgs) ->
    io:format("STOMP Q Broker started~n"),
    qerl_supervisor:start_link(StartArgs).
stop(_State) -> ok.
go() -> application:start(qerl_application).


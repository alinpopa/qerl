-module(qerl_app).
-behaviour(application).
-export([start/2,stop/1,go/0]).

start(_Type, StartArgs) ->
    io:format("STOMP Q Broker started~n"),
    case qerl_supervisor:start_link(StartArgs) of
      {ok, Pid} -> {ok, Pid};
      Other -> {error, Other}
    end.
stop(_State) -> ok.
go() ->
  application:start(qerl_application).


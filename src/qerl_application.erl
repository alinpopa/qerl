-module(qerl_application).
-behaviour(application).
-export([start/0,stop/1]).

start() -> qerl_supervisor:start_link([]).
stop(_State) -> ok.


-module(qerl_example_server).
-export([start/0]).

start() -> application:start(qerl_application).


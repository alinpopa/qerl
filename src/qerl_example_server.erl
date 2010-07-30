-module(qerl_example_server).
-export([start/0,stop/0]).

start() -> qerl_socket_server:start(7000).
stop() -> qerl_socket_server:stop().


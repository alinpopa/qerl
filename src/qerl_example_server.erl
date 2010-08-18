-module(qerl_example_server).
-export([start/0]).

start() -> qerl_conn_manager:start(qerl_conn_listener,7000,10).


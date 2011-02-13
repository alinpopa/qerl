{application, qerl_application,
    [{description,"STOP Q broker"}
     {vsn, "0.1"},
     {modules, [qerl_conn_manager, qerl_conn_listener, qerl_stomp_fsm, qerl_stomp_protocol]},
     {registered, []},
     {applications, []},
     {mod, {qerl_application,[]}}
    ]}.


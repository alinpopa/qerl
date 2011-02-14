{application, qerl_application,
    [
        {description,"STOMP Q broker"},
        {vsn, "0.1"},
        {modules, [qerl_application, qerl_supervisor, qerl_conn_manager, qerl_conn_listener, qerl_stomp_fsm, qerl_stomp_protocol]},
        {registered, []},
        {applications, [
            kernel,
            stdlib
        ]},
        {included_applications, []},
        {env, []},
        {mod, {qerl_application,[]}}
    ]
}.


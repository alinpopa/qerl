{application, qerl,
 [{description, "MQ Broker"},
  {vsn, "0.01"},
  {modules, [
		qerl_stomp,
		qerl_tcp,
        qerl_server
        ]}, 
  {registered, []},
  {mod, {qerl_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.


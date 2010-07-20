%%% The contents of this file are Open Source.  They may be freely
%%% copied, used or enhanced in any way without restriction provided
%%% acknowledgement of the original author is included in all
%%% distributions of any derivative works.  This file is intended
%%% as an example tutorial only and is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
%%% The software is not guaranteed to work, nor is the author
%%% liable for any damage or lost opportunity caused by the use of
%%% this software.
%%% 
%%%-------------------------------------------------------------------
%%% File    : tcp_proxy.erl
%%% Author  : Jay Nelson <jay@duomark.com>
%%% Description : A proxy server works on behalf of another process.
%%%
%%%   Date    Auth    Desc
%%% 03-03-15   JN   Initial creation
%%% 03-04-13   JN   Read from socket when too busy to allow sending
%%%-------------------------------------------------------------------
-module(tcp_proxy).
-behaviour(gen_server).
-compile([verbose, report_errors, report_warnings, trace, debug_info]).

-define(TCP_OPTIONS,[binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(MSGPORT, 8080).
-define(MAX_CONNECTS, 3).
-define(CLIENT_TAB, "_tcp_proxy_clients").

% TCP Proxy server maintains its state in a single record
-record(tp_state,
	{tcp_opts = ?TCP_OPTIONS,  % TCP Socket options
	 port,                     % Port connections accepted on
	 listen,                   % The listen socket instance
	 accept_pid,               % The current accept Process Id
	 accept_deaths = 0,        % The number of times accept EXITed
	 clients,                  % ETS table of currently active clients
	 client_count = 0,         % Current number of active requests
	 max_active = 0,           % Maximum number of active allowed
	 requests = 0,             % Total number of requests handled
	 server_busy = 0,          % Number of times server was busy
	 pmodule,                  % ProxyModule name
	 pm_state                  % ProxyModule's dynamic state
	}).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, start_link/2, start_link/3, start_link/5,
	 report_clients/1, handle_request/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		    terminate/2, code_change/3]).


%%--------------------------------------------------------------------
%% Function: start_link/1, /2, /3 and /4
%% Description: Starts the gen_server and listens for socket connects.
%%
%% Returns:  {ok, Pid}        |
%%           {error, Reason}
%%--------------------------------------------------------------------
start_link(ProxyModule) ->
    start_link(ProxyModule, ProxyModule, ?MSGPORT, ?MAX_CONNECTS,
	       list_to_atom(atom_to_list(ProxyModule) ++ ?CLIENT_TAB)).
start_link(ProxyModule, MaxConnects) ->
    start_link(ProxyModule, ProxyModule, ?MSGPORT, MaxConnects,
	       list_to_atom(atom_to_list(ProxyModule) ++ ?CLIENT_TAB)).
start_link(ProxyModule, Port, MaxConnects) ->
    start_link(ProxyModule, ProxyModule, Port, MaxConnects,
	       list_to_atom(atom_to_list(ProxyModule) ++ ?CLIENT_TAB)).

start_link(ServerName, ProxyModule, Port, MaxConnects, ClientTab) ->
    case gen_server:start_link({local, ServerName}, ?MODULE,
			       [ProxyModule,Port,MaxConnects,ClientTab], []) of
	{ok, Pid} ->
	    start_listen(Pid, Port);
	{error, {already_started, OldPid}} ->
	    {ok, OldPid};
	Error ->
	    error_logger:error_report([{start_link, Error}]),
	    Error
    end.


%%--------------------------------------------------------------------
%% Function: report_clients/1
%% Description: Provide a list of the attached clients.
%%
%% Returns: ok
%%--------------------------------------------------------------------
report_clients(Server) ->
    gen_server:call(Server, report_clients).


%%--------------------------------------------------------------------
%% Function: handle_request/3
%% Description: Respond to a request for a ProxyModule function to run.
%%
%% Returns: ok
%%--------------------------------------------------------------------
handle_request(Server, Request, Data) ->
    gen_server:call(Server, {msg, Request, Data}).


%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stop handling requests and shutdown
%%
%% Returns: {stop, requested}
%%--------------------------------------------------------------------
stop(Server) ->
    gen_server:call(Server, stop).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description:
%%   Initiates the server with TCP Options to specify the packet send
%%   and receive formats, as well as the name of the ProxyModule so
%%   that requests can be delegated properly.
%%
%% Returns: {ok, State} |
%%          {stop, Error}
%%--------------------------------------------------------------------
init([ProxyModule, Port, MaxConnects, ClientTab]) ->

    % Let ProxyModule initialize, then setup State and return.
    case catch ProxyModule:init() of
	{ok, ProxyState} ->
	    process_flag(trap_exit, true),
	    {ok, #tp_state{port = Port, pmodule = ProxyModule,
			   pm_state = ProxyState, max_active = MaxConnects,
			   clients = ets:new(ClientTab, [named_table])}};
	Error ->
	    {stop, Error}
    end.


%%--------------------------------------------------------------------
%% Function: start_listen/3
%% Description: Starts listening for network requests on Port.
%%
%% Returns:  {ok, Pid}       |
%%           {error, Reason}
%%--------------------------------------------------------------------
start_listen(Pid, Port) ->
    case gen_server:call(Pid, {listen, Port}) of
	ok ->
	    start_accept(Pid);
	Error ->
	    error_logger:error_report([{start_listen, Error}]),
	    Error
     end.


%%--------------------------------------------------------------------
%% Function: start_accept/1
%% Description: Starts a new process to accept client connections
%%
%% Returns:  {ok, Pid}       |
%%           {error, Reason}
%%--------------------------------------------------------------------
start_accept (Pid) when is_pid(Pid) ->
    case gen_server:call(Pid, {accept, Pid}) of
	ok ->
	    {ok, Pid};
	Other ->
	    error_logger:error_report([{start_accept, Other}]),
	    Other
    end.


%% Start up causes the listen port to be opened
%% Returns: {reply, ok, NewState}                Listening on port
%%          {stop, {error, Reason}, State}       Port could not open
%%--------------------------------------------------------------------
handle_call({listen, NewPort}, _From,
	    #tp_state{port = OldPort, tcp_opts = TcpOpts} = State) ->

    % Open a new listen port
    case catch gen_tcp:listen(NewPort, TcpOpts) of

	% If we succeed, modify the state (possibly closing the old
	% listen port) and return a successful reply
	{ok, LSocket} ->
	    NewPortState = State#tp_state{port = NewPort, listen = LSocket},
	    case OldPort of
		undefined ->
		    {reply, ok, NewPortState};
		NewPort ->
		    {reply, ok, NewPortState};

		% Close the old port, but don't pay attention to result
		_Other ->
		    Table = State#tp_state.clients,
		    ets:delete_all_objects(Table),
		    gen_tcp:close(OldPort),
		    {reply, ok, NewPortState#tp_state{client_count = 0}}
	    end;

	% If the call fails, tell the caller and don't change state or
	% close the existing listen port since terminate/2 will finish
	NotOK ->
	    {stop, NotOK, State}
    end;


%% Handle the initial request to start accepting network connections
%% Returns: {reply, ok, NewState}
%%--------------------------------------------------------------------
handle_call({accept, Server}, _From, #tp_state{accept_pid = undefined,
					       listen = LSocket} = State) ->

    Pid = spawn_link(fun() -> accept(Server, LSocket) end),
    {reply, ok, State#tp_state{accept_pid = Pid}};


%% Handle a new client connection by replacing the accept with a new
%% process and then deciding whether to refuse the new client connect.
%% Returns: {reply, ok, NewState}
%%          {reply, {error, too_many_connections}, NewState}
%%--------------------------------------------------------------------
handle_call({connect, Pid, _Socket, Server}, _From,
	    #tp_state{accept_pid = Pid, client_count = Connects,
		      max_active = Max, clients = Table,
		      listen = LSocket, requests = Requests,
		      pmodule = ProxyModule, pm_state = _ProxyState} = State) ->
    
    % Start a new accept process
    NewAcceptPid = spawn_link(fun() -> accept(Server, LSocket) end),
    erlang:unlink(Pid),      % Avoids handle_info when too many connected
    case Connects < Max of

	% Keep the new client but change to a lightweight monitor
	true ->
	    MonitorRef = erlang:monitor(process, Pid),

            % Add the client process to the set of current connections
	    ets:insert(Table, {MonitorRef, Pid}),
	    {reply, {ok, ProxyModule},
	     State#tp_state{accept_pid = NewAcceptPid,
			    client_count = Connects + 1,
			    requests = Requests + 1}};

	% Too many connections are currently active
	false ->
	    Busy = State#tp_state.server_busy,
	    {reply, {error, too_many_connections, ProxyModule},
	     State#tp_state{accept_pid = NewAcceptPid, requests = Requests+1,
			    server_busy = Busy + 1}}
    end;


%% Handle a request that should be passed to ProxyModule.
%% Returns:  {reply, ok, NewState}
%%           {stop, {error, Reason}, NewState}
%%--------------------------------------------------------------------
handle_call({msg, Request, Data}, From,
	    #tp_state{pmodule = ProxyModule, pm_state = PMState} = State) ->

    case catch ProxyModule:Request(From, PMState, Data) of
	{ok, NewProxyState} ->
	    {reply, ok, State#tp_state{pm_state = NewProxyState}};

	% Report failure in other cases, but don't take server down
	Other ->
	    Fun = atom_to_list(ProxyModule)
		++ ":" ++ atom_to_list(Request) ++ "/3",
	    error_logger:error_report([{Fun, Other}, {'State', State}]),

	    % Only return a new state if the ProxyModule detected and updated
	    case Other of
		{error, _Error, NewPMState} ->
		    {reply, ok, State#tp_state{pm_state=NewPMState}};
		_Other ->
		    {reply, ok, State}
	    end
    end;


%% Handle a request to get information about the current clients
%% and statistics on the server performance.
%% Returns:  {reply, {ok, Count, Clients}, NewState}
%%           {stop, {error, Reason}, NewState}
%%--------------------------------------------------------------------
handle_call(report_clients, _From,
	    #tp_state{pmodule = ProxyModule, accept_deaths = Deaths,
		      client_count = Active, clients = Table,
		      max_active = Max, requests = Connects,
		      server_busy = Busy} = State) ->

    {reply, {ok, [{proxy_module, ProxyModule}, {active_clients, Active},
		  {clients, ets:tab2list(Table)}, {max_active_clients, Max},
		  {total_requests, Connects}, {server_busy, Busy},
		  {accept_failures, Deaths}]},
     State};


%% Handle a request to shutdown
%% Returns:  {stop, requested, State}
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, requested, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling non-blocking requests for service
%%--------------------------------------------------------------------
% handle_cast(Request, State)

%% All other casts are unexpected and are logged and ignored
%% Returns:  {noreply, State}
%%--------------------------------------------------------------------
handle_cast(Cast, State) ->
    error_logger:info_report([{'CAST', Cast}, {'State', State}]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
% handle_info(Info, State)

%% If the accept process dies, clear the Pid in the State and spawn a
%% new accept process.
%% Returns: {noreply, State}
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason},
	    #tp_state{accept_pid = Pid, accept_deaths = Deaths,
		      listen = LSocket} = State) ->

    error_logger:error_report([{'Accept EXIT', Reason}, {'State', State}]),
    ServerId = self(),
    NewPid = spawn_link(fun() -> accept(ServerId, LSocket) end),
    {noreply, State#tp_state{accept_pid = NewPid, accept_deaths = Deaths + 1}};


%% When any monitored client goes down, remove it from the client set
%% Returns: {noreply, State}
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info},
	    #tp_state{clients = Table, client_count = OldCount} = State) ->

    case ets:member(Table, MonitorRef) of
	true ->
	    ets:delete(Table, MonitorRef),
	    {noreply, State#tp_state{client_count = OldCount - 1}};
	false ->
	    {noreply, State}
    end;


%% All other unexpected messages are logged and ignored
%% Returns: {noreply, State}
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server by closing the TCP connection.
%%
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, #tp_state{pmodule = ProxyModule, pm_state = PMState,
			    listen = Port}) ->

    % Shutdown the ProxyModule; the client table removes itself
    ProxyModule:terminate(PMState),

    % Closing the listen socket kills any Accept socket processes and clients
    case Port of
	undefined -> ok;
	Port -> gen_tcp:close(Port)
    end.


%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    error_logger:info_report([{'CODE-CHANGE-OLDVSN', OldVsn},
			      {'State', State}, {'Extra', Extra}]),
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: accept/2
%% Description:
%%    Accept a client request on the listen port, notify the server
%%    that one has arrived and then convert the connection to a
%%    ProxyModule pocess.  The server will create a new Accept process.
%%
%% Returns: None
%%--------------------------------------------------------------------
accept(Server, LSocket) ->

    % Wait for a client to connect
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->

            % Request a new accept socket then start ProxyModule response
	    Result = gen_server:call(Server, {connect, self(),Socket,Server}),
	    case Result of
		{ok, ProxyModule} ->
		    relay(Server, ProxyModule, Socket);

                % Let ProxyModule issue a server busy message
		{error, too_many_connections, ProxyModule} ->
		    % Socket refuses to accept data if it hasn't been read
		    gen_tcp:recv(Socket, 0, 1000),
		    ProxyModule:server_busy(Socket),
		    gen_tcp:close(Socket)
	    end;

         % Just let this process die if gen_tcp:accept fails
	 % The handle_info callback will notice and restart it.
	NotOK ->
	    error_logger:info_report([{"gen_tcp:accept", NotOK}])
    end.


%%--------------------------------------------------------------------
%% Function: relay/3
%% Description:
%%    Given an accepted socket, read the data on the socket and give
%%    it to the proxy module.
%%
%% Returns: None
%%--------------------------------------------------------------------
relay(Server, ProxyModule, Socket) ->

    % Get the full TCP request
    case gen_tcp:recv(Socket, 0) of

        % Let the ProxyModule deal with it
	{ok, BinData} ->
	    case catch ProxyModule:react_to(Server, Socket, BinData) of
		ok -> ok;
		{'EXIT', ok} -> ok;
		{'EXIT', Reason} ->
		    error_logger:info_report([{atom_to_list(ProxyModule) ++
					       ":react_to/3 failed", Reason}]);
		Other ->
		    error_logger:info_report([{atom_to_list(ProxyModule) ++
					       ":react_to/3 failed", Other}])
	    end;

        % Log any Socket receive problem
	NotOK ->
	    error_logger:info_report([{"gen_tcp:recv/2", NotOK}])
    end,

    % Close the socket in all cases
    gen_tcp:close(Socket).

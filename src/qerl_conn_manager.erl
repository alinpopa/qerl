-module(qerl_conn_manager).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/2,detach/0]).

-record(server_state,{socket,listeners_size}).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(LISTENER_MODULE,qerl_conn_listener).

%%
%% API functions
%%
start_link(Port,ListenersSize) ->
  gen_server:start_link({local,?MODULE},?MODULE,[Port,ListenersSize],[]).

detach() -> gen_server:call(?MODULE,detach).

spawn_listeners(_,0) -> ok;
spawn_listeners(ListeningSocket,ListenersSize) ->
    ?LISTENER_MODULE:start_link(ListeningSocket),
    spawn_listeners(ListeningSocket,ListenersSize-1).

%%
%% Callback functions
%%
init([Port,ListenersSize]) ->
    case gen_tcp:listen(Port,?TCP_OPTIONS) of
        {ok,ListeningSocket} ->
            ok = spawn_listeners(ListeningSocket,ListenersSize),
            error_logger:info_msg("Server started successful on port: ~p, listeners size: ~p~n",[Port,ListenersSize]),
            {ok,#server_state{socket=ListeningSocket,listeners_size=ListenersSize}};
        {error,Reason} ->
            {stop,Reason}
    end.

handle_call(detach,{From,_Ref}, State = #server_state{socket=ListeningSocket}) ->
    ok = spawn_listeners(ListeningSocket,1),
    unlink(From),
    {reply,ok,State};
handle_call(stop,_From,State) -> {stop,normal,State};
handle_call(_Request,_From,State) ->
    Reply = ok,
    {reply,Reply,State}.

handle_cast(_Msg,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


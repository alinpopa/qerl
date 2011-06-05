-module(qerl_conn_listener).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/1,stop/1,send_to_client/2]).

-import(qerl_stomp_utils, [drop/2]).

-record(conn_state, {listening_socket, client_socket, data = <<>>, frames = [], fsm, parser, tcp_filters}).

-define(LISTENER_MANAGER,qerl_conn_manager).
-define(STOMP,qerl_stomp_protocol).
-define(STOMP_FSM,qerl_stomp_fsm).
-define(STOMP_PARSER,qerl_stomp_frame_parser).
-define(TCP_FILTERS,qerl_tcp_filters).

%%
%% API functions
%%
start_link(ListeningSocket) -> gen_server:start_link(?MODULE,[ListeningSocket],[]).

stop(Pid) -> gen_server:cast(Pid,{close}).
send_to_client(Pid,Msg) -> gen_server:cast(Pid, {send_to_client,Msg}).

trace(Msg) -> io:format("~p: ~p~n",[?MODULE,Msg]).

%%
%% Callback functions
%%
init([ListeningSocket]) ->
    {ok,FsmPid} = ?STOMP_FSM:start_link([self()]),
    {ok,ParserPid} = ?STOMP_PARSER:start_link(),
    {ok,TcpFilters} = ?TCP_FILTERS:start_link(),
    gen_server:cast(self(), {listen}),
    {ok,#conn_state{listening_socket = ListeningSocket, fsm = FsmPid, parser = ParserPid, tcp_filters = TcpFilters}}.

handle_cast({listen}, State) ->
    {ok,ClientSocket} = gen_tcp:accept(State#conn_state.listening_socket),
    io:format(" -> client connected (current no. of processes: ~p)~n",[length(processes())]),
    gen_tcp:controlling_process(ClientSocket,self()),
    inet:setopts(ClientSocket, [{active, once}]),
    ?LISTENER_MANAGER:detach(),
    {noreply,State#conn_state{client_socket = ClientSocket, data = <<>>, frames = []}};
handle_cast({close},State) ->
    gen_tcp:close(State#conn_state.client_socket),
    {stop,normal,State};
handle_cast({send_to_client,MsgToClient},State) ->
    gen_tcp:send(State#conn_state.client_socket,[MsgToClient ++ [10,10,0]]),
    {noreply,State};
handle_cast(Msg,State) -> {noreply,State}.

handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_info({tcp,ClientSocket,Bin},State) ->
  Parser = State#conn_state.parser,
  Filters = State#conn_state.tcp_filters,
  ParseReply = ?STOMP_PARSER:parse(Parser,?TCP_FILTERS:apply(Filters,Bin)),
  %io:format("Got reply from parser: ~p~n",[ParseReply]),
  BinData = State#conn_state.data,
  NewBinData = <<BinData/binary, Bin/binary>>,
  inet:setopts(State#conn_state.client_socket, [{active, once}]),
  case ?STOMP:is_eof(NewBinData) of
      true ->
          Parsed = ?STOMP:parse(NewBinData),
          ?STOMP_FSM:process(State#conn_state.fsm, Parsed),
          {noreply,State#conn_state{data = <<>>}};
      _ ->
          {noreply,State#conn_state{data = drop(null,NewBinData)}}
  end;
handle_info({tcp_closed,_ClientSocket},State) ->
    ?STOMP_FSM:stop(State#conn_state.fsm),
    {stop,normal,State};
handle_info(Info,State) ->
    {noreply,State}.

terminate(_Reason,State) ->
  ?TCP_FILTERS:stop(State#conn_state.tcp_filters),
  ?STOMP_PARSER:stop(State#conn_state.parser),
  ok.
code_change(_OldVsn, State, _Extra) -> {ok,State}.


-module(qerl_conn_listener).
-behaviour(gen_server).

-export([init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]).
-export([start_link/1,stop/1,send_to_client/2]).

-import(qerl_stomp_utils, [drop/2]).

-record(conn_state, {listening_socket, client_socket, data = <<>>, frames = [], fsm, tcp_filters}).

-define(LISTENER_MANAGER,qerl_conn_manager).
-define(STOMP,qerl_stomp_protocol).
-define(STOMP_FSM,qerl_stomp_fsm).
-define(TCP_FILTERS,qerl_tcp_filters).
-define(NULL,0).
-define(LF,10).

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
    {ok,TcpFilters} = ?TCP_FILTERS:start_link(),
    gen_server:cast(self(), {listen}),
    {ok,#conn_state{listening_socket = ListeningSocket, fsm = FsmPid, tcp_filters = TcpFilters}}.

handle_cast({listen}, State) ->
    {ok,ClientSocket} = gen_tcp:accept(State#conn_state.listening_socket),
    error_logger:info_msg("Client connected (current no. of processes: ~p)~n",[length(processes())]),
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

handle_info({tcp,ClientSocket,TcpBin},State) ->
  Filters = State#conn_state.tcp_filters,
  Bin = ?TCP_FILTERS:apply(Filters,TcpBin),
  BinData = State#conn_state.data,
  NewBinData = <<BinData/binary, Bin/binary>>,
  inet:setopts(State#conn_state.client_socket, [{active, once}]),
  case check_and_process(NewBinData,State) of
    <<>> -> {noreply,State#conn_state{data = <<>>}};
    Rest -> {noreply,State#conn_state{data = Rest}}
  end;
handle_info({tcp_closed,_ClientSocket},State) ->
    ?STOMP_FSM:stop(State#conn_state.fsm),
    {stop,normal,State};
handle_info(Info,State) ->
    {noreply,State}.

terminate(_Reason,State) ->
  ?TCP_FILTERS:stop(State#conn_state.tcp_filters),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok,State}.

process_frames(FsmProcessor,[]) -> ok;
process_frames(FsmProcessor,[Frame|Rest]) ->
  Parsed = ?STOMP:parse(Frame),
  ?STOMP_FSM:process(FsmProcessor, Parsed),
  process_frames(FsmProcessor,Rest).

is_eof(<<>>) -> false;
is_eof(Data) ->
  case binary:match(Data,<<?LF,?LF>>) of
    nomatch -> false;
    _ ->
      case binary:match(Data,<<?NULL>>) of
        nomatch -> false;
        _ -> true
      end
  end.

valid_frames([]) -> [];
valid_frames(Frames) ->
  [Frame||Frame <- Frames, Frame =/= <<>>].

parse_frames(<<>>) -> {{rest,<<>>},{complete,[]}};
parse_frames(Data) ->
  SplitData = binary:split(Data,<<?NULL>>,[global]),
  {{rest,Rest},{complete,Frames}} = parse_frames(SplitData,[]),
  ValidFrames = lists:reverse(valid_frames(Frames)),
  case Rest of
    <<?LF>> -> {{rest,<<>>},{complete,ValidFrames}};
    _ -> {{rest,Rest},{complete,ValidFrames}}
  end.

parse_frames([],ParsedFrames) ->
  {{rest,<<>>},{complete,ParsedFrames}};
parse_frames([IncompleteFrame|[]], ParsedFrames) ->
  {{rest,IncompleteFrame},{complete,ParsedFrames}};
parse_frames([Frame,<<>>], ParsedFrames) ->
  parse_frames([],[Frame|ParsedFrames]);
parse_frames([Frame|Rest], ParsedFrames) ->
  parse_frames(Rest, [Frame|ParsedFrames]).

check_and_process(Data,State) ->
  case is_eof(Data) of
    true ->
      case parse_frames(Data) of
        {{rest,Rest},{complete,ParsedFrames}} ->
          process_frames(State#conn_state.fsm, ParsedFrames),
          Rest;
        ParsingResponse ->
          error_logger:error_msg("Wrong response for parsing: ~p~n",[ParsingResponse]),
          <<>>
      end;
    _ ->
      Data
  end.


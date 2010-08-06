-module(qerl_stomp_protocol).
-export([is_eof/1, parse_frame/1, parse/1]).
-import(string, [str/2]).
-import(lists, [splitwith/2, flatten/1]).
-import(re, [split/2]).
-import(qerl_utils, [strip_cr/1, trim/1, bin_to_list/1]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).

%% Check if the passed list contains an end of frame.
%%%is_eof(ListData) ->
%%%    End = str(strip_cr(ListData),[10,0]),
%%%    End =/= 0.

is_eof(Bin) ->
    case binary:match(drop_cr(Bin),<<?LF,?NULL>>) of
        nomatch -> false;
        _ -> true 
    end.

%% Parse the given frame and return a structure having the format:
%% {{command, []}, {headers, [[]]}, {body, []}}
parse_frame(RawListData) ->
    ListData = strip_cr(RawListData),
    {Command, Rest} = splitwith(fun(X) -> X =/= 10 end, ListData),
    [Headers|Body] = proc_rest(Rest),
    {{command, Command},{headers, proc_headers(Headers)}, {body, proc_body(Body)}}.

proc_rest(RestData) ->
    Rest = split(RestData, [10,10]),
    bin_to_list(Rest).

proc_headers(Headers) ->
    BinH = split(Headers, [10]),
    bin_to_list(BinH).

proc_body(Body) ->
    trim(flatten(Body)).

parse(Bin) ->
    LfBin = drop_cr(Bin),
    parse_msg(binary:split(LfBin,<<?LF>>,[global])). 

parse_msg([<<"CONNECT">>|T]) -> {connect, {headers,get_headers(T)}};
parse_msg([<<"DISCONNECT">>|T]) -> {disconnect,T};
parse_msg(Bin) -> {something_else,Bin}.

drop_cr(Bin) when is_binary(Bin) -> binary:replace(Bin,<<?CR>>,<<>>,[global]);
drop_cr(_Bin) -> <<>>.

drop_lf(Bin) when is_binary(Bin) -> binary:replace(Bin,<<?LF>>,<<>>,[global]);
drop_lf(_Bin) -> <<>>.

is_valid(Element) ->
    case Element of
        <<>> -> false;
        <<?NULL>> -> false;
        _ -> true
    end.

get_headers(BinList) -> get_headers(BinList,[]).

get_headers([],Headers) -> Headers;
get_headers([H|T],Headers) ->
    case H of
        <<>> -> Headers;
        Else -> get_headers(T,[Else|Headers])
    end.


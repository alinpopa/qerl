-module(qerl_protocol).
-export([is_eof/1, parse_frame/1]).
-import(string, [str/2]).
-import(lists, [splitwith/2, flatten/1]).
-import(re, [split/2]).
-import(qerl_utils, [strip_cr/1, trim/1, bin_to_list/1]).

%% Check if the passed list contains an end of frame.
is_eof(ListData) ->
    End = str(strip_cr(ListData),[10,0]),
    End =/= 0.

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


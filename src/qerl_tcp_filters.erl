-module(qerl_tcp_filters).
-export([apply/1]).

-define(CR,13).
-define(NULL,0).

-define(FILTERS,[
  fun(Data) -> drop(cr,Data) end
]).

apply(<<>>) -> <<>>;
apply(Data) when is_binary(Data) ->
  lists:foldl(fun(F,Data) -> F(Data) end, Data, ?FILTERS).

drop(_,<<>>) -> <<>>;
drop(What,Bin) when is_binary(Bin) ->
    case What of
        cr -> binary:replace(Bin,<<?CR>>,<<>>,[global]);
        null -> binary:replace(Bin,<<?NULL>>,<<>>,[global]);
        _ -> Bin
    end;
drop(_What,_Bin) -> <<>>.


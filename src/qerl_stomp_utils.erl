-module(qerl_stomp_utils).
-export([drop/2]).

-import(binary, [replace/4]).

-define(LF,10).
-define(CR,13).
-define(NULL,0).

%% Drop specific byte from the given binary
drop(_,<<>>) -> <<>>;
drop(What,Bin) when is_binary(Bin) ->
    case What of
        cr -> replace(Bin,<<?CR>>,<<>>,[global]);
        lf -> replace(Bin,<<?LF>>,<<>>,[global]);
        null -> replace(Bin,<<?NULL>>,<<>>,[global]);
        _ -> Bin
    end;
drop(_What,_Bin) -> <<>>.


-module(qsort).
-author("wojciech").

%% API
-export([qs/1]).

lessThan(List, Arg) ->
    [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
    [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

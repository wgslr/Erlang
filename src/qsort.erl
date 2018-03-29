-module(qsort).
-author("wojciech").

%% API
-export([qs/1]).
-export([randomElems/3]).
-export([compareSpeeds/3]).
-export([map/2, filter/2, foldl/3]).

lessThan(List, Arg) ->
    [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
    [X || X <- List, X >= Arg].


-spec qs([T]) -> [T].
qs([]) -> [];
qs([Pivot | Tail]) ->
    qs(lessThan(Tail, Pivot))
    ++ [Pivot]
        ++ qs(grtEqThan(Tail, Pivot)).


-spec randomElems(non_neg_integer(), Min :: integer(), Max :: integer()) -> integer().
randomElems(N, Min, Max) ->
    Dist = Max - Min + 1,
    [rand:uniform(Dist) - 1 + Min || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
    {Time1, Result1} = timer:tc(Fun1, [List]),
    {Time2, Result1} = timer:tc(Fun2, [List]),
    {Time1, Time2}.

map(Fun, List) ->
    [Fun(X) || X <- List].

filter(Predicate, List) ->
    [X || X <- List, Predicate(X) == true].

foldl(_, Acc0, []) -> Acc0;
foldl(Fun, Acc0, [H|T]) ->
    foldl(Fun, Fun(H, Acc0), T).


-module(pingpong).
-author("wojciech").

%% API
-export([start/0, stop/0, play/1]).

-record(state,{
    name :: atom(),
    opponent :: atom(),
    msg_left :: non_neg_integer()
}).



start() ->
    register(ping, spawn(fun() -> player(pong) end)),
    register(pong, spawn(fun() -> player(ping) end)).

stop() ->
    ping ! pong ! stop.

play(N) ->
    ping ! N.


player(Other) ->
    receive
        stop ->
            io:format("~p stopping~n", [self()]),
            ok;
        Left when is_integer(Left) ->
            io:format("~p received ~p~n", [self(), Left]),
            timer:sleep(200),

            case Left > 0 of
                true ->
                    io:format("~p throwing to ~s, ~b left~n", [self(), Other, Left - 1]),
                    Other ! Left - 1,
                    player(Other);
                false ->
                    io:format("~p waiting for game~n", [self()]),
                    player(Other)
            end;
        Msg -> io:format("Unexpected message in ~p: ~p~n", [self(), Msg])
    after
        20000 -> io:format("Exiting after 20 seconds~n")
    end.


%%player(#state{msg_left = N, name = Name} = State) when N =< 0 ->
%%    receive
%%        N when is_integer(N) ->
%%            io:format("~s awaken~n", [Name]),
%%            State#state.opponent ! {ball, N},
%%            player(State#state{msg_left = N})
%%    after
%%        20000 ->
%%            io:format("Exiting after 20 seconds~n")
%%    end.
%%player(#state{name = Name, msg_left = MsgLeft} = State) ->
%%    receive
%%        {ball, Num} ->
%%            io:format("~s received ball~n", [Name]),
%%            timer:sleep(200),
%%            io:format("~s throwing ball~n", [Name]),
%%            State#state.opponent ! ball,
%%            player(State#state{msg_left = MsgLeft - 1});
%%        N ->
%%            io:format("~s awaken~n", [Name]),
%%            State#state.opponent ! ball,
%%            player(State#state{msg_left = N})
%%    after
%%        20000 ->
%%            io:format("Exiting after 20 seconds~n")
%%    end.

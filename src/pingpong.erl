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
    register(ping, spawn(fun() ->
        player(#state{name = ping, opponent = pong, msg_left = inf}) end)),
    register(pong, spawn(fun() ->
            player(#state{name = pong, opponent = ping, msg_left = inf}) end)).

stop() ->
    erlang:error(not_implemented).

play(N) ->
    ping ! N,
    pong ! N.

player(#state{msg_left = 0}) ->
    timer:sleep(timer:seconds(20));
player(#state{name = Name, msg_left = MsgLeft} = State) ->
    receive
        ball ->
            io:format("~s received ball~n", [Name]),
            timer:sleep(200),
            io:format("~s throwing ball~n", [Name]),

            State#state.opponent ! ball,
            player(State#state{msg_left = MsgLeft - 1});
        N ->
            io:format("~s awaken~n", [Name]),
            State#state.opponent ! ball,
            player(State#state{msg_left = N})
    after
        20000 ->
            io:format("Exiting after 20 seconds~n")
    end.

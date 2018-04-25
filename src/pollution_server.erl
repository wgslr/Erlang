-module(pollution_server).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

%% API
-export([start/0, stop/0]).
-export([call/1, call/2]).

%%%===================================================================
%%% Public API
%%%===================================================================

start() ->
    register(?MODULE, spawn(fun init/0)).

stop() ->
    ?MODULE ! {stop, self()}.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec call(term()) -> term() | {error, timeout}.
call(Request) ->
    call(Request, timer:seconds(1)).
-spec call(term(), non_neg_integer() | infinity) -> term() | {error, timeout}.
call(Request, Timeout) ->
    ?MODULE ! {Request, self()},
    receive
        Resp -> Resp
    after
        Timeout -> {error, timeout}
    end.

%%%===================================================================
%%% Server Internals
%%%===================================================================

init() ->
    loop(#monitor{}).


loop(State) ->
    receive
        {Request, From} ->
            {Response, NewState} = handle(Request, State),
            From ! Response,
            loop(NewState);
        Malformed ->
            io:format("Server received malformed request: ~p", [Malformed]),
            loop(State)
    end.


handle(Arg, State) ->
    {{error, bad_request}, State}.

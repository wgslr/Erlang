-module(pollution_server_sup).
-author("Wojciech Geisler").

%% API
-export([start/0]).


start() ->
    register(?MODULE, spawn(fun init/0)).


init() ->
    process_flag(trap_exit, true),
    io:format("Starting pollution_server~n", []),
    pollution_gen_server:start_link(),
    receive
        {'EXIT', _From, Reason} ->
            io:format("Supervised process died because of ~p~n", [Reason]),
            init()
    end.


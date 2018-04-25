-module(pollution_server).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

-type id() :: {name, station_name()} | {coord, coord()}.
-type state() :: monitor().

%% API
-export([start/0, stop/0]).
-export([addStation/2, addValue/4, getOneValue/3, removeValue/3]).

-export([get_state/0]).
-export([call/1, call/2]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start() -> ok.
start() ->
    register(?MODULE, spawn(fun init/0)),
    ok.


-spec stop() -> state().
stop() ->
    io:format("Sending stop~n", []),
    ?MODULE ! {stop, self()},
    receive
        {stopped, State} -> State
    after
        1000 -> {error, timeout}
    end.


-spec addStation(Name :: station_name(), Coord :: coord()) ->
    ok | {error, exists} | {error, timeout}.
addStation(Name, Coord) ->
    call({add_station, Name, Coord}).


-spec addValue(id(), timestamp(), kind(), float()) ->
    ok | {error, Reason :: term()}.
addValue(CoordOrName, Datetime, MeasureKind, Value) ->
    call({add_value, CoordOrName, Datetime, MeasureKind, Value}).


-spec getOneValue(id(), timestamp(), kind()) ->
    datapoint() | {error, not_found | bad_station}.
getOneValue(CoordOrName, Time, Kind) ->
    call({get_value, CoordOrName, Time, Kind}).


-spec removeValue(id(), timestamp(), kind()) -> ok | {error, Reason :: term()}.
removeValue(CoordOrName, Datetime, Kind) ->
    call({remove_value, CoordOrName, Datetime, Kind}).


-spec get_state() -> state().
get_state() ->
    call(get_state).

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec call(term()) -> term() | {error, timeout}.
call(Request) ->
    call(Request, timer:seconds(1)).
-spec call(Request :: term(), Timeout :: non_neg_integer() | infinity) ->
    Response :: term() | {error, timeout}.
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


-spec loop(state()) -> state().
loop(State) ->
    io:format("State: ~p", [State]),
    receive
        {stop, From} ->
            From ! {stopped, State},
            State;
        {Request, From} ->
            io:format("Request ~p at ~p~n", [{Request, From}, State]),
            {Response, NewState} = handle(Request, State),
            From ! Response,
            loop(NewState);
        Malformed ->
            io:format("~s received malformed request: ~p", [?MODULE, Malformed]),
            loop(State)
    end.

-spec handle(Request :: term(), State :: state()) -> state().
handle(get_state, State) ->
    {State, State};

handle({add_station, Name, Coord}, State) ->
    case pollution:addStation(Name, Coord, State) of
        #monitor{} = M -> {ok, M};
        {error, _} = Error -> {Error, State}
    end;

handle({add_value, CoordOrName, Datetime, MeasureKind, Value}, State) ->
    case pollution:addValue(CoordOrName, Datetime, MeasureKind, Value, State) of
        #monitor{} = M -> {ok, M};
        {error, _} = Error -> {Error, State}
    end;

handle({get_value, CoordOrName, Time, Kind}, State) ->
    {pollution:getOneValue(CoordOrName, Time, Kind, State), State};

handle({remove_value, CoordOrName, Datetime, Kind}, State) ->
    case pollution:removeValue(CoordOrName, Datetime, Kind, State) of
        #monitor{} = M -> {ok, M};
        Error -> {Error, State}
    end;

handle(_Request, State) ->
    {{error, bad_request}, State}.




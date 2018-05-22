-module(pollution_server).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

-type id() :: {name, station_name()} | {coord, coord()}.
-type state() :: monitor().

-define(ETS_BACKUP, pollution_state).
-define(KEY, key).

%% API
-export([start/0, start_link/0, stop/0]).
-export([addStation/2, addValue/4, getOneValue/3, removeValue/3]).
-export([getStationMean/2, getDailyMean/2, getAirQualityIndex/2]).
-export([crash/0]).
-export([get_state/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start() -> ok.
start() ->
    register(?MODULE, spawn(fun init/0)),
    ok.


-spec start_link() -> ok.
start_link() ->
    register(?MODULE, spawn_link(fun init/0)),
    ok.


-spec stop() -> state().
stop() ->
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


-spec getStationMean(id(), kind()) ->
    float() | undefined | {error, bad_station}.
getStationMean(CoordOrName, Type) ->
    call({get_station_mean, CoordOrName, Type}).


-spec getDailyMean(calendar:date(), kind()) -> float() | undefined.
getDailyMean(Date, Type) ->
    call({get_daily_mean, Date, Type}).


-spec getAirQualityIndex(CoordOrName :: id(), Datetime :: timestamp()) ->
    integer() | {error, no_data} | {error, bad_station}.
getAirQualityIndex(CoordOrName, Datetime) ->
    call({get_aqi, CoordOrName, Datetime}).


-spec crash() -> no_return().
crash() ->
    call(crash).


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
    receive
        {stop, From} ->
            From ! {stopped, State},
            State;
        {Request, From} ->
            {Response, NewState} = handle(Request, State),
            From ! Response,
            loop(NewState);
        Malformed ->
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

handle({remove_value, CoordOrName, Datetime, Kind}, State) ->
    case pollution:removeValue(CoordOrName, Datetime, Kind, State) of
        #monitor{} = M -> {ok, M};
        Error -> {Error, State}
    end;

handle({get_value, CoordOrName, Time, Kind}, State) ->
    {pollution:getOneValue(CoordOrName, Time, Kind, State), State};

handle({get_station_mean, CoordOrName, Kind}, State) ->
    {pollution:getStationMean(CoordOrName, Kind, State), State};

handle({get_daily_mean, Date, Kind}, State) ->
    {pollution:getDailyMean(Date, Kind, State), State};

handle({get_aqi, CoordOrName, Datetime}, State) ->
    {pollution:getAirQualityIndex(CoordOrName, Datetime, State), State};

handle(crash, State) ->
    {inf/banana, State};

handle(_Request, State) ->
    {{error, bad_request}, State}.




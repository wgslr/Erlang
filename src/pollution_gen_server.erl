-module(pollution_gen_server).
-author("Wojciech Geisler").

-behavior(gen_server).

-include("../include/pollution.hrl").

-type id() :: {name, station_name()} | {coord, coord()}.
-type state() :: monitor().

% gen_Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% API
-export([start_link/0, stop/0]).
-export([addStation/2, addValue/4, getOneValue/3, removeValue/3]).
-export([getStationMean/2, getDailyMean/2, getAirQualityIndex/2]).
-export([crash/0]).
-export([get_state/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link() -> ok.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec stop() -> state().
stop() ->
    gen_server:stop(?MODULE).


-spec addStation(Name :: station_name(), Coord :: coord()) ->
    ok | {error, exists} | {error, timeout}.
addStation(Name, Coord) ->
    gen_server:call(?MODULE, {add_station, Name, Coord}).


-spec addValue(id(), timestamp(), kind(), float()) ->
    ok | {error, Reason :: term()}.
addValue(CoordOrName, Datetime, MeasureKind, Value) ->
    gen_server:call(?MODULE, {add_value, CoordOrName, Datetime, MeasureKind, Value}).


-spec getOneValue(id(), timestamp(), kind()) ->
    datapoint() | {error, not_found | bad_station}.
getOneValue(CoordOrName, Time, Kind) ->
    gen_server:call(?MODULE, {get_value, CoordOrName, Time, Kind}).


-spec removeValue(id(), timestamp(), kind()) -> ok | {error, Reason :: term()}.
removeValue(CoordOrName, Datetime, Kind) ->
    gen_server:call(?MODULE, {remove_value, CoordOrName, Datetime, Kind}).


-spec getStationMean(id(), kind()) ->
    float() | undefined | {error, bad_station}.
getStationMean(CoordOrName, Type) ->
    gen_server:call(?MODULE, {get_station_mean, CoordOrName, Type}).


-spec getDailyMean(calendar:date(), kind()) -> float() | undefined.
getDailyMean(Date, Type) ->
    gen_server:call(?MODULE, {get_daily_mean, Date, Type}).


-spec getAirQualityIndex(CoordOrName :: id(), Datetime :: timestamp()) ->
    integer() | {error, no_data} | {error, bad_station}.
getAirQualityIndex(CoordOrName, Datetime) ->
    gen_server:call(?MODULE, {get_aqi, CoordOrName, Datetime}).


-spec crash() -> no_return().
crash() ->
    gen_server:cast(?MODULE, crash).


-spec get_state() -> state().
get_state() ->
    gen_server:call(?MODULE, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #monitor{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({add_station, Name, Coord}, _From, State) ->
    case pollution:addStation(Name, Coord, State) of
        #monitor{} = M -> {reply, ok, M};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call({add_value, CoordOrName, Datetime, MeasureKind, Value}, _From, State) ->
    case pollution:addValue(CoordOrName, Datetime, MeasureKind, Value, State) of
        #monitor{} = M -> {reply, ok, M};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call({remove_value, CoordOrName, Datetime, Kind}, _From, State) ->
    case pollution:removeValue(CoordOrName, Datetime, Kind, State) of
        #monitor{} = M -> {reply, ok, M};
        Error -> {reply, Error, State}
    end;

handle_call({get_value, CoordOrName, Time, Kind}, _From, State) ->
    {reply, pollution:getOneValue(CoordOrName, Time, Kind, State), State};

handle_call({get_station_mean, CoordOrName, Kind}, _From, State) ->
    {reply, pollution:getStationMean(CoordOrName, Kind, State), State};

handle_call({get_daily_mean, Date, Kind}, _From, State) ->
    {reply, pollution:getDailyMean(Date, Kind, State), State};

handle_call({get_aqi, CoordOrName, Datetime}, _From, State) ->
    {reply, pollution:getAirQualityIndex(CoordOrName, Datetime, State), State};

handle_call(_Request, _From, State) ->
    io:format("~s received unexpected call: ~p~n", [?MODULE, _Request]),
    {reply, {error, bad_request}, State}.


handle_cast(crash, State) ->
    Result = (inf / banana),
    {noreply, State};

handle_cast(Request, State) ->
    io:format("~s received unexpected cast: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Request, State) ->
    io:format("~s received unexpected info: ~p~n", [?MODULE, Request]),
    {noreply, State}.

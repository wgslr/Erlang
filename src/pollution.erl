-module(pollution).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

%%-type sensor_name() :: string().
-type id() :: {name, station_name()} | {coord, coord()}.

-export_type([station_name/0]).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,getOneValue/4, getStationMean/3,
    getDailyMean/3]).


%%-compile(export_all).

-spec createMonitor() -> monitor().
createMonitor() ->
    #monitor{}.


-spec addStation(Name :: station_name(), Coord :: coord(), monitor()) ->
    {ok, monitor()} | {error, exists}.
addStation(Name, Coord, M) ->
    #monitor{coord_to_name = CtN, name_to_station = NtS} = M,
    case {findStation({name, Name}, M), findStation({coord,Coord}, M)}  of
        {{error, not_found}, {error, not_found}} ->
            Station = #station{coord = Coord, name = Name},
            {ok, M#monitor{
                coord_to_name = CtN#{Coord => Name},
                name_to_station = NtS#{Name => Station}
            }};
        _ -> {error, exists}
    end.


-spec addValue(id(), timestamp(), kind(), float(), monitor()) ->
    {ok, monitor()} | {error, exists}.
addValue(CoordOrName, Datetime, MeasureKind, Value, M) ->
    {ok, #station{data = Data} = S} = findStation(CoordOrName, M),
    case exists(Datetime, MeasureKind, Data) of
        true -> {error, exists};
        false ->
            Point = buildPoint(Datetime, MeasureKind, Value),
            {ok, updateStation(S#station{data = [Point | Data]}, M)}
    end.


-spec removeValue(id(), timestamp(), kind(), monitor()) -> monitor().
removeValue(CoordOrName, Datetime, Kind, M) ->
    {ok, S} = findStation(CoordOrName, M),
    Point = buildPoint(Datetime, Kind),
    Data = lists:filter(fun(P) ->
        not(spacetimeEquals(Point, P))
    end, S#station.data),
    updateStation(S#station{data = Data}, M).


% typ, data, stacja
-spec getOneValue(id(), timestamp(), kind(), monitor()) ->
    datapoint() | {error, not_found}.
getOneValue(CoordOrName, Time, Kind, M) ->
    Point = buildPoint(Time, Kind),
    {ok, #station{data = Data}} = findStation(CoordOrName, M),
    case lists:filter(fun(P) ->
        spacetimeEquals(Point, P) end, Data
    ) of
        [Found] -> Found;
        [] -> {error, not_found}
    end.


-spec getStationMean(id(), kind(), monitor()) -> float().
getStationMean(CoordOrName, Type, M) ->
    {ok, #station{data = Data}} = findStation(CoordOrName, M),
    FilteredData = lists:filter(fun({_, T, _}) -> T =:= Type end, Data),
    lists:sum(lists:map(fun({_, _, Value}) -> Value end, FilteredData))
        / length(FilteredData).


% typ, dzien
getDailyMean(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).


-spec findStation(id(), monitor()) -> {ok, station()} | {error, not_found}.
findStation({name, Name}, #monitor{name_to_station = NtS}) ->
    case maps:get(Name, NtS, undefined) of
        undefined -> {error, not_found};
        #station{} = S -> {ok, S}
    end;

findStation({coord, Coord}, #monitor{coord_to_name = CtN, name_to_station = NtS}) ->
    case maps:get(Coord, CtN, undefined) of
        undefined -> {error, not_found};
        Name -> {ok, maps:get(Name, NtS)}
    end.


-spec buildPoint(timestamp(), kind()) -> datapoint().
buildPoint(Time, Kind) ->
    {Time, Kind, 0.0}.

-spec buildPoint(timestamp(), kind(), number()) -> datapoint().
buildPoint(Time, Kind, Value) ->
    {Time, Kind, float(Value)}.


%% Updates station data.
%% Name and coordinate must not change.
-spec updateStation(station(), monitor()) -> monitor().
updateStation(S, #monitor{name_to_station = NtS} = M) ->
    M#monitor{
        name_to_station = NtS#{S#station.name := S}
    }.


% Checks if value exists with the same Kind and Datetime
-spec exists(timestamp(), kind(), [datapoint()]) -> boolean().
exists(Datetime, Kind, Dataset) ->
    Point = buildPoint(Datetime, Kind),
    lists:any(fun(P) ->
        spacetimeEquals(Point, P)
    end, Dataset).


% Compares datapoints by Kind and Time
-spec spacetimeEquals(datapoint(), datapoint()) -> boolean().
spacetimeEquals({T, K, _}, {T, K, _}) -> true;
spacetimeEquals(_, _) -> false.


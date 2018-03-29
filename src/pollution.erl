-module(pollution).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

%%-type sensor_name() :: string().
-type id() :: {name, station_name()} | {coord, coord()}.

-export_type([station_name/0]).

-export([addStation/3, addValue/5, removeValue/4,getOneValue/4, getStationMean/3, getDailyMean/3]).


-compile(export_all).

createMonitor() ->
    #monitor{}.


-spec addStation(Name :: station_name(), Coord :: coord(), #monitor{}) -> #monitor{}.
addStation(Name, Coord, M) ->
    #monitor{coord_to_name = CtN, name_to_station = NtS} = M,
    case {findStation({name, Name}, M), findStation({coord,Coord}, M)}  of
        {{error, not_found}, {error, not_found}} ->
            M#monitor{
                coord_to_name = CtN#{Coord => Name},
                name_to_station = CtN#{Name => #station{
                    coord = Coord, name = Name, data = []
                }}
            };
        _ -> {error, exists}
    end.


-spec addValue(id(), timestamp(), kind(), float(), #monitor{}) ->
    #monitor{} | no_return().
addValue(CoordOrName, Datetime, MeasureKind, Value, M) ->
    #station{data = Data} = S = findStation(CoordOrName, M),
    case exists(Datetime, MeasureKind, Data)of
        true -> {error, exists};
        false ->
            Point = buildPoint(Datetime, MeasureKind, Value),
            updateStation(S#station{data = [Point | Data]}, M)
    end.



%addValue(CoordOrName, Date, MeasureKind, Monitor) ->
-spec removeValue(id(), timestamp(), kind(), monitor()) -> monitor().
removeValue(CoordOrName, Datetime, Kind, M) ->
    {ok, S} = findStation(CoordOrName, M),
    Point = buildPoint(Datetime, Kind),
    Data = lists:filter(fun(P) ->
        not(spacetimeEquals(Point, P))
    end, S#station.data),
    updateStation(S#station{data = Data}, M).


% typ, data, stacja
getOneValue(_Arg0, _Arg1, _Arg2, _Arg3) ->
    erlang:error(not_implemented).

%

% typ, stacj
getStationMean(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).

% typ, dzien
getDailyMean(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).


-spec findStation(id(), #monitor{}) -> {ok, #station{}} | {error, not_found}.
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


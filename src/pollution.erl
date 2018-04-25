-module(pollution).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

%%-type sensor_name() :: string().
-type id() :: {name, station_name()} | {coord, coord()}.

-export_type([station_name/0]).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
    getStationMean/3, getDailyMean/3, getAirQualityIndex/3]).


%%%===================================================================
%%% Public API
%%%===================================================================

-spec createMonitor() -> monitor().
createMonitor() ->
    #monitor{}.


-spec addStation(Name :: station_name(), Coord :: coord(), monitor()) ->
    monitor() | {error, exists}.
addStation(Name, Coord, M) ->
    #monitor{coord_to_name = CtN, name_to_station = NtS} = M,
    case isStationAvailable(Name, Coord, M) of
        true ->
            Station = #station{coord = Coord, name = Name},
            M#monitor{
                coord_to_name = CtN#{Coord => Name},
                name_to_station = NtS#{Name => Station}
            };
        false -> {error, exists}
    end.


-spec addValue(id(), timestamp(), kind(), float(), monitor()) ->
    monitor() | {error, exists} | {error, bad_station}.
addValue(CoordOrName, Datetime, MeasureKind, Value, M) ->
    case findStation(CoordOrName, M) of
        {ok, #station{data = Data} = S} ->
            case exists(Datetime, MeasureKind, Data) of
                true -> {error, exists};
                false ->
                    Point = buildPoint(Datetime, MeasureKind, Value),
                    updateStation(S#station{data = [Point | Data]}, M)
            end;
        _ -> {error, bad_station}
    end.


-spec removeValue(id(), timestamp(), kind(), monitor()) ->
    monitor() | {error, bad_station}.
removeValue(CoordOrName, Datetime, Kind, M) ->
    case findStation(CoordOrName, M) of
        {ok, S} ->
            Point = buildPoint(Datetime, Kind),
            Data = lists:filter(fun(P) ->
                not(spacetimeEquals(Point, P))
            end, S#station.data),
            updateStation(S#station{data = Data}, M);
        _ -> {error, bad_station}
    end.


-spec getOneValue(id(), timestamp(), kind(), monitor()) ->
    datapoint() | {error, not_found} | {error, bad_station}.
getOneValue(CoordOrName, Time, Kind, M) ->
    Point = buildPoint(Time, Kind),
    case findStation(CoordOrName, M) of
        {ok, #station{data = Data}} ->
            case lists:filter(fun(P) ->
                spacetimeEquals(Point, P) end, Data
            ) of
                [Found] -> Found;
                [] -> {error, not_found}
            end;
        _ -> {error, bad_station}
    end.


-spec getStationMean(id(), kind(), monitor()) ->
    float() | undefined | {error, bad_station}.
getStationMean(CoordOrName, Type, M) ->
    case findStation(CoordOrName, M) of
        {ok, #station{data = Data}} ->
            Values = lists:filtermap(fun({_, T, V}) ->
                case T =:= Type of
                    true -> {true, V};
                    _ -> false
                end
            end, Data),
            mean(Values);
        _ -> {error, bad_station}
    end.


-spec getDailyMean(calendar:date(), kind(), monitor()) -> float() | undefined.
getDailyMean(Date, Type, #monitor{name_to_station = NtS}) ->
    Values = lists:flatmap(fun({_Name, #station{data = Data}}) ->
        lists:filtermap(fun({{D, _}, T, V}) ->
            case D =:= Date andalso T =:= Type of
                true -> {true, V};
                _ -> false
            end
        end, Data)
    end, maps:to_list(NtS)),
    mean(Values).


-spec getAirQualityIndex(CoordOrName :: id(), Datetime :: timestamp(),
    M :: monitor()) -> integer() | {error, no_data} | {error, bad_station}.
getAirQualityIndex(CoordOrName, Datetime, M) ->
    case findStation(CoordOrName, M) of
        {ok, #station{data = Data}} ->
            Relevant = lists:filtermap(fun({D, T, V}) ->
                case D =:= Datetime of
                    true -> {true, {T, V}};
                    false -> false
                end
            end, Data),
            case lists:filtermap(fun({Type, Value}) ->
                case qualityIndex(Type, Value) of
                    undefined -> false;
                    Index -> {true, Index}
                end
            end, Relevant) of
                [] -> {error, no_data};
                Values -> lists:max(Values)
            end;
        _ -> {error, bad_station}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Checks if Name and Coordinates are not already registered in monitor
-spec isStationAvailable(Name :: station_name(), Coord :: coord(), monitor()) ->
    boolean().
isStationAvailable(Name, Coord, M) ->
    case {findStation({name, Name}, M), findStation({coord, Coord}, M)} of
        {{error, not_found}, {error, not_found}} -> true;
        _ -> false
    end.


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


-spec mean([float()]) -> float() | undefined.
mean([]) -> undefined;
mean(List) -> lists:sum(List) / length(List).


-spec getNorm(kind()) -> float() | undefined.
getNorm("PM10") -> 50.0;
getNorm("PM2.5") -> 30.0;
getNorm("PM2,5") -> getNorm("PM2.5");
getNorm(_) -> undefined.


qualityIndex(Type, Value) ->
    case getNorm(Type) of
        undefined -> undefined;
        Norm -> round(Value * 100 / Norm)
    end.

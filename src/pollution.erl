-module(pollution).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

%%-type sensor_name() :: string().
-type id() :: {name, station_name()} | {coord, coord()}.

-export_type([station_name/0]).


%% API
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
    case exists(MeasureKind, Datetime, Data)of
        true -> {error, exists};
        false -> M#monitor{
            name_to_station = S#station{
                data = [{MeasureKind, Datetime, Value} | Data]
            }}
    end.


% Checks if value exists with the same Kind and Datetime
-spec exists(kind(), timestamp(), [datapoint()]) -> boolean().
exists(Kind, Datetime, Dataset) ->
    lists:any(fun({Kind2, Datetime2, _}) ->
        case {Kind2, Datetime2} of
            {Kind, Datetime} -> true;
            _ -> false
        end
    end, Dataset).


%addValue(CoordOrName, Date, MeasureKind, Monitor) ->
-spec removeValue(id(), timestamp(), kind(), monitor()) -> monitor().
removeValue(CoordOrName, Datetime, Kind, M) ->
    {ok, S} = findStation(CoordOrName),
    Data = S#station.data,
    Data2 = lists:filter(fun({Kind2, Datetime2,_} ->
    )

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
    end
findStation({coord, Coord}, #monitor{coord_to_name = CtN, name_to_station = NtS}) ->
    case maps:get(Coord, CtN, undefined) of
        undefined -> {error, not_found};
        Name -> {ok, maps:get(Name, NtS)}
    end.

pointEquals({K, T, _}, {K, T, _}) -> true;
pointEquals(_, _) -> false.

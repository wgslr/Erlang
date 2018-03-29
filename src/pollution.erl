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
    #monitor{coord_to_name = CtN, name_to_senor = NtS} = Monitor,
    case {findStation({name, Name}, M), findStation({coord,Coord}, M)}  of
        {{error, not_found}, {error, not_found}} ->
            M#monitor{
                coord_to_name = CtN#{Coord => Name},
                name_to_senor = CtN#{Name => #station{
                    coord = Coord, name = Name, data = []
                }}
            };
        _ -> {error, exists}
    end.



addValue(CoordOrName, Date, MeasureKind, Valye, Monitor) ->
    erlang:error(not_implemented).

%addValue(CoordOrName, Date, MeasureKind, Monitor) ->
removeValue(_Arg0, _Arg1, _Arg2, _Arg3) ->
    erlang:error(not_implemented).

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


-spec findStation(id(), #monitor{}) -> #station{} | {error, not_found}.
findStation({name, Name}, #monitor{name_to_senor = NtS}) ->
    maps:get(Name, NtS, {error, not_found});
findStation({coord, Coord}, #monitor{coord_to_name = CtN, name_to_senor = NtS}) ->
    case maps:get(Coord, CtN, undefined) of
        undefined -> {error, not_found};
        Name -> maps:get(Name, NtS)
    end.

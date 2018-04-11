-module(pollution_test).
-author("wojciech").

-include("pollution.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STATION_NAME1, "station1").
-define(STATION_NAME2, "Krakow2").
-define(STATION_COORD1, {23.3, -5.0}).
-define(STATION_COORD2, {50.06, 19.95}).
-define(STATION_COORD_ZERO, {0.0, 0.0}).
-define(DATA_TIME1, {{2018, 04, 11}, {11, 12, 13}}).
-define(DATA_TIME2, {{2018, 05, 11}, {12, 00, 13}}).
-define(DATA_TYPE1, "PM10").
-define(DATA_TYPE2, "PM2,5").
-define(DATA_VALUE1, 3.4).
-define(DATA_VALUE2, 7.4).


create_monitor_test() ->
    ?assertEqual(#monitor{}, pollution:createMonitor()).

addStation_test() ->
    M = pollution:createMonitor(),
    ?assertMatch({ok, _}, pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M)).

addStation_fails_on_duplicate_test() ->
    % Setup
    M = pollution:createMonitor(),
    {ok, M2} = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M),

    % Test
    ?assertMatch({error, _}, pollution:addStation(
        ?STATION_NAME1, ?STATION_COORD1, M2
    )).

addValue_test_() ->
    M1 = pollution:createMonitor(),
    {ok, M2} = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    {ok, M3} = pollution:addStation(?STATION_NAME2, ?STATION_COORD2, M2),
    M = M3,
    [
        % Station by name
        ?_assertMatch({ok, #monitor{}}, pollution:addValue(
            {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)),
        % Station by coord name
        ?_assertMatch({ok, #monitor{}}, pollution:addValue(
            {coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)),
        % Station by coord name and coord give equal result
        ?_assertEqual(
            pollution:addValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M),
            pollution:addValue({name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)
        )
    ].


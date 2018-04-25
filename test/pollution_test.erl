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

-define(DATAPOINT1, {?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1}).
-define(ANY_DATAPOINT, {_, _, _}).


create_monitor_test() ->
    ?assertEqual(#monitor{}, pollution:createMonitor()).

addStation_test() ->
    M = pollution:createMonitor(),
    ?assertMatch(#monitor{}, pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M)).

addStation_fails_on_duplicate_test() ->
    % Setup
    M = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M),

    % Test
    ?assertMatch({error, _}, pollution:addStation(
        ?STATION_NAME1, ?STATION_COORD1, M2
    )).

addValue_test_() ->
    M1 = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    M = M2,
    [
        % Station by name
        ?_assertMatch(#monitor{}, pollution:addValue(
            {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)),
        % Station by coord name
        ?_assertMatch(#monitor{}, pollution:addValue(
            {coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)),
        % Station by coord name and coord give equal result
        ?_assertEqual(
            pollution:addValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M),
            pollution:addValue({name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)
        ),
        % Created value can be found
        fun() ->
            NewM = pollution:addValue(
                {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M),
            ?assertEqual(?DATAPOINT1,
                pollution:getOneValue({name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, NewM)
            )
        end,

        {"addValue_fail_on_duplicate", fun() ->
            NewM = pollution:addValue(
                {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M),
            ?assertEqual({error, exists},
                pollution:addValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, NewM))
        end},

        {"addValue_fail_on_missing_station", fun() ->
            ?assertError(_,
                pollution:addValue({coord, ?STATION_COORD2}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M)),
            ?assertError(_,
                pollution:addValue({name, ?STATION_NAME2}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M))
        end}
    ].

removeValue_test() ->
    % setup
    M1 = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    M3 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1, M2),
    M = M3,

    ?assertMatch(?ANY_DATAPOINT, pollution:getOneValue({coord, ?STATION_COORD1}, ?DATA_TIME1,
        ?DATA_TYPE1, M)),
    NewM = #monitor{} = pollution:removeValue({coord, ?STATION_COORD1}, ?DATA_TIME1,
        ?DATA_TYPE1, M),
    ?assertMatch({error, not_found}, pollution:getOneValue({coord, ?STATION_COORD1}, ?DATA_TIME1,
        ?DATA_TYPE1, NewM)).

getStationMean_test_() ->
    [V1, V2, V3] = [2.0, 8.0, 16.5],
    Expected = (V1 + V2) / 2,

    M1 = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    M3 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, ?DATA_TYPE1, V1, M2),
    M4 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME2, ?DATA_TYPE1, V2, M3),
    M5 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME2, ?DATA_TYPE2, V3, M4),
    M = M5,

    [
        {"getStationMean_empty_test",
        ?_assertEqual(undefined, pollution:getStationMean({coord, ?STATION_COORD1}, ?DATA_TYPE1, M2))},
        ?_assertEqual(Expected, pollution:getStationMean({coord, ?STATION_COORD1}, ?DATA_TYPE1, M))
    ].



getDailyMean_test() ->
    [V1, V2, V3] = Vals = [2.0, 8.0, 16.5],
    Expected = lists:sum(Vals) / 3,
    OtherTypeVal = 100.0,
    OtherDayVal = 200.0,
    Day = {2018, 04, 10},
    OtherDay = {2018, 04, 11},
    Time1 = {10, 42, 0},
    Time2 = {11, 42, 0},

    M1 = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    M3 = pollution:addStation(?STATION_NAME2, ?STATION_COORD2, M2),

    M4 = pollution:addValue({coord, ?STATION_COORD1},
        {Day, Time1}, ?DATA_TYPE1, V1, M3),
    M5 = pollution:addValue({coord, ?STATION_COORD1},
        {Day, Time2}, ?DATA_TYPE1, V2, M4),
    M6 = pollution:addValue({coord, ?STATION_COORD2},
        {Day, Time1}, ?DATA_TYPE1, V3, M5),

    M7 = pollution:addValue({coord, ?STATION_COORD1},
        {Day, Time1}, ?DATA_TYPE2, OtherTypeVal, M6),
    M8 = pollution:addValue({coord, ?STATION_COORD1},
        {OtherDay, Time1}, ?DATA_TYPE1, OtherDayVal, M7),

    M = M8,

    ?assertEqual(Expected, pollution:getDailyMean(Day, ?DATA_TYPE1, M)).


getAirQualityIndex_test() ->
    [PM10, PM25] = [75, 60],
    Expected = 200,

    M1 = pollution:createMonitor(),
    M2 = pollution:addStation(?STATION_NAME1, ?STATION_COORD1, M1),
    M3 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "PM10", PM10, M2),
    M4 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "PM2,5", PM25, M3),
    M5 = pollution:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "Other,5", 5, M4),
    M = M5,

    ?assertEqual(Expected, pollution:getAirQualityIndex(
        {coord, ?STATION_COORD1}, ?DATA_TIME1, M)).



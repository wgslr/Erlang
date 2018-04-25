-module(pollution_server_test).
-author("Wojciech Geisler").

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

-define(SETUP(__TEST), {setup, fun setup/0, fun teardown/1, __TEST}).

setup() ->
    pollution_server:start(),
    ok.


teardown(_) ->
    pollution_server:stop().


main_test_() -> [
    {"Start registers, stop unregisters2",
        fun start_registers_stop_unregisters/0},
    {foreach, fun setup/0, fun teardown/1, lists:flatten([
        {"Default state is empty monitor2",
            fun default_state_is_monitor/0},
        {"addStation fails on duplicate",
            fun addStation_fails_on_duplicate/0},
        {"Removed value cannot be retrieved",
            fun removeValue/0},
        {"getDailyMean returns daily mean",
            fun getDailyMean/0},
        {"getAirQualiyIndex returns AQI",
            fun getAirQualityIndex/0},
        getStationMean_tests()
    ])}].


addValue_test_() ->
    Setup = fun() ->
        setup(),
        pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1)
    end,
    {foreach, Setup, fun teardown/1, [
        {"Value can be added to station specified by name",
            ?_assertMatch(ok, pollution_server:addValue(
                {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1))},

        {"Value can be added to station specified by coord",
            ?_assertMatch(ok, pollution_server:addValue(
                {coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1))},

        {"Added measurement can be retrieved",
            fun() ->
                pollution_server:addValue(
                    {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1),
                ?assertEqual(?DATAPOINT1,
                    pollution_server:getOneValue({name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1)
                )
            end},

        %% Fail on duplicate
        {"addValue fails on duplicate", fun() ->
            NewM = pollution_server:addValue(
                {name, ?STATION_NAME1}, ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1),
            ?assertEqual({error, exists},
                pollution_server:addValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1,
                    ?DATA_VALUE1))
        end},

        %% Fail on duplicate
        {"addValue fails on missing station", fun() ->
            ?assertMatch({error, bad_station},
                pollution_server:addValue({coord, ?STATION_COORD2}, ?DATA_TIME1, ?DATA_TYPE1,
                    ?DATA_VALUE1)),
            ?assertMatch({error, bad_station},
                pollution_server:addValue({name, ?STATION_NAME2}, ?DATA_TIME1, ?DATA_TYPE1,
                    ?DATA_VALUE1))
        end}
    ]}.

removeValue() ->
    % setup
    pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),
    pollution_server:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, ?DATA_TYPE1, ?DATA_VALUE1),

    ?assertMatch(?ANY_DATAPOINT, pollution_server:getOneValue({coord, ?STATION_COORD1}, ?DATA_TIME1,
        ?DATA_TYPE1)),
    pollution_server:removeValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1),
    ?assertMatch({error, not_found},
        pollution_server:getOneValue({coord, ?STATION_COORD1}, ?DATA_TIME1, ?DATA_TYPE1)).


getStationMean_tests() ->
    [V1, V2, V3] = [2.0, 8.0, 16.5],
    Expected = (V1 + V2) / 2,

    [
        {"getStationMean returns undefined for empty set",
            fun() ->
                pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),
                ?assertEqual(undefined, pollution_server:getStationMean(
                    {coord, ?STATION_COORD1}, ?DATA_TYPE1))
            end},
        {"getStationMean calculates mean",
            fun() ->
                pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),
                pollution_server:addValue({coord, ?STATION_COORD1},
                    ?DATA_TIME1, ?DATA_TYPE1, V1),
                pollution_server:addValue({coord, ?STATION_COORD1},
                    ?DATA_TIME2, ?DATA_TYPE1, V2),
                pollution_server:addValue({coord, ?STATION_COORD1},
                    ?DATA_TIME2, ?DATA_TYPE2, V3),
                ?assertEqual(Expected, pollution_server:getStationMean(
                    {coord, ?STATION_COORD1}, ?DATA_TYPE1))

            end
        }
    ].


start_registers_stop_unregisters() ->
    pollution_server:start(),
    ?assert(lists:member(pollution_server, registered())),
    pollution_server:stop(),
    ?assertNot(lists:member(pollution_server, registered())).


default_state_is_monitor() ->
    ?assertEqual(#monitor{}, pollution_server:get_state()).


addStation_fails_on_duplicate() ->
    pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),

    % Test
    ?assertMatch({error, _}, pollution_server:addStation(
        ?STATION_NAME1, ?STATION_COORD1
    )).


getDailyMean() ->
    [V1, V2, V3] = Vals = [2.0, 8.0, 16.5],
    Expected = lists:sum(Vals) / 3,
    OtherTypeVal = 100.0,
    OtherDayVal = 200.0,
    Day = {2018, 04, 10},
    OtherDay = {2018, 04, 11},
    Time1 = {10, 42, 0},
    Time2 = {11, 42, 0},

    pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),
    pollution_server:addStation(?STATION_NAME2, ?STATION_COORD2),

    pollution_server:addValue({coord, ?STATION_COORD1},
        {Day, Time1}, ?DATA_TYPE1, V1),
    pollution_server:addValue({coord, ?STATION_COORD1},
        {Day, Time2}, ?DATA_TYPE1, V2),
    pollution_server:addValue({coord, ?STATION_COORD2},
        {Day, Time1}, ?DATA_TYPE1, V3),

    pollution_server:addValue({coord, ?STATION_COORD1},
        {Day, Time1}, ?DATA_TYPE2, OtherTypeVal),
    pollution_server:addValue({coord, ?STATION_COORD1},
        {OtherDay, Time1}, ?DATA_TYPE1, OtherDayVal),

    ?assertEqual(Expected, pollution_server:getDailyMean(Day, ?DATA_TYPE1)).


getAirQualityIndex() ->
    [PM10, PM25] = [75, 60],
    Expected = 200,

    pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),
    pollution_server:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "PM10", PM10),
    pollution_server:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "PM2,5", PM25),
    pollution_server:addValue({coord, ?STATION_COORD1},
        ?DATA_TIME1, "Other,5", 5),

    ?assertEqual(Expected, pollution_server:getAirQualityIndex(
        {coord, ?STATION_COORD1}, ?DATA_TIME1)).


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
    {foreach, fun setup/0, fun teardown/1, [
        {"Default state is empty monitor2",
            fun default_state_is_monitor/0},
        {"addStation fails on duplicate",
            fun addStation_fails_on_duplicate/0},
        {"Removed value cannot be retrieved",
            fun removeValue/0}
    ]}].


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

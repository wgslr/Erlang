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

all_test_() ->
   {foreach, fun setup/0, fun teardown/1, {module, ?MODULE}}.

setup() ->
    pollution_server:start(),
    ok.

teardown(_) ->
    pollution_server:stop().

start_registers_stop_unregsiters_test() ->
%%    pollution_server:start(),
    ?assert(lists:member(pollution_server, registered())),
    pollution_server:stop(),
    ?assertNot(lists:member(pollution_server, registered())).


default_state_is_monitor_test() ->
%%    pollution_server:start(),
    ?assertEqual(#monitor{}, pollution_server:get_state()),
    pollution_server:stop().


addStation_fails_on_duplicate_test() ->
    % Setup

%%    pollution_server:start(),
    pollution_server:addStation(?STATION_NAME1, ?STATION_COORD1),

    % Test
    ?assertMatch({error, _}, pollution_server:addStation(
        ?STATION_NAME1, ?STATION_COORD1
    )).

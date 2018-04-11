-module(pollution_test).
-author("wojciech").

-include("pollution.hrl").
-include_lib("eunit/include/eunit.hrl").

create_monitor_test() ->
    ?assertEqual(#monitor{}, pollution:createMonitor()).


-author("Wojciech Geisler").

-ifndef(POLLUTION_HRL).
-define(POLLUTION_HRL, 1).

-type station_name() :: string().
-type coord() :: {float(), float()}.
-type kind() :: string().
-type timestamp() :: calendar:datetime().
-type datapoint() :: {kind(), calendar:datetime(), float()}.


%%-record(datapoint,{
%%    datetime :: calendar:datetime(),
%%    value :: double(),
%%    kind :: kind()
%%}).

-record(station, {
    name :: station_name(),
    coord :: coord(),
    data = [] :: [datapoint()]
}).
-type station() :: #station{}.

-record(monitor, {
    name_to_station = #{} :: #{station_name() => #station{}},
    coord_to_name = #{} :: #{coord() => station_name()}
}).
-type monitor() :: #monitor{}.

-endif.

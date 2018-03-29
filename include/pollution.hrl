-author("Wojciech Geisler").

-ifndef(POLLUTION_HRL).
-define(POLLUTION_HRL, 1).

-type station_name() :: string().
-type coord() :: {float(), float()}.
-type kind() :: pm10 | pm25.


-record(datapoint,{
    datetime :: calendar:datetime(),
    value :: double(),
    kind :: kind()
}).

-record(station, {
    name :: station_name(),
    coord :: coord(),
    data :: [#datapoint{}]
}).

-record(monitor, {
    name_to_senor = #{} :: #{station_name() => #station{}},
    coord_to_name = #{} :: #{coord() => station_name()}
}).

-endif.

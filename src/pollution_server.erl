-module(pollution_server).
-author("Wojciech Geisler").

-include("../include/pollution.hrl").

-type id() :: {name, station_name()} | {coord, coord()}.
-type state() :: monitor().

%% API
-export([start/0, stop/0]).
-export([addStation/2]).

-export([get_state/0]).
-export([call/1, call/2]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start() -> ok.
start() ->
    register(?MODULE, spawn(fun init/0)),
    ok.


-spec stop() -> state().
stop() ->
    io:format("Sending stop~n", []),
    ?MODULE ! {stop, self()},
    receive
        {stopped, State} -> State
    after
        1000 -> {error, timeout}
    end.


-spec addStation(Name :: station_name(), Coord :: coord()) ->
    ok | {error, exists} | {error, timeout}.
addStation(Name, Coord) ->
    call({add_station, Name, Coord}).


-spec get_state() -> state().
get_state() ->
    call(get_state).

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec call(term()) -> term() | {error, timeout}.
call(Request) ->
    call(Request, timer:seconds(1)).
-spec call(Request :: term(), Timeout :: non_neg_integer() | infinity) ->
    Response :: term() | {error, timeout}.
call(Request, Timeout) ->
    ?MODULE ! {Request, self()},
    receive
        Resp -> Resp
    after
        Timeout -> {error, timeout}
    end.

%%%===================================================================
%%% Server Internals
%%%===================================================================

init() ->
    loop(#monitor{}).


-spec loop(state()) -> state().
loop(State) ->
    io:format("State: ~p", [State]),
    receive
        {stop, From} ->
            From ! {stopped, State},
            State;
        {Request, From} ->
            io:format("Request ~p at ~p~n", [{Request, From}, State]),
            {Response, NewState} = handle(Request, State),
            From ! Response,
            loop(NewState);
        Malformed ->
            io:format("~s received malformed request: ~p", [?MODULE, Malformed]),
            loop(State)
    end.

-spec handle(Request :: term(), State :: state()) -> state().
handle(get_state, State) ->
    {State, State};

handle({add_station, Name, Coord}, State) ->
    #monitor{coord_to_name = CtN, name_to_station = NtS} = State,
    case isStationAvailable(Name, Coord, State) of
        true ->
            Station = #station{coord = Coord, name = Name},
            NewState = State#monitor{
                coord_to_name = CtN#{Coord => Name},
                name_to_station = NtS#{Name => Station}
            },
            {ok, NewState};
        false -> {{error, exists}, State}
    end;

handle(_Request, State) ->
    {{error, bad_request}, State}.

%% Checks if Name and Coordinates are not already registered in monitor
    -spec isStationAvailable(Name :: station_name(), Coord :: coord(), monitor()) ->
boolean().
isStationAvailable(Name, Coord, M) ->
    case {findStation({name, Name}, M), findStation({coord, Coord}, M)} of
        {{error, not_found}, {error, not_found}} -> true;
        _ -> false
    end.


-spec findStation(id(), monitor()) -> {ok, station()} | {error, not_found}.
findStation({name, Name}, #monitor{name_to_station = NtS}) ->
    case maps:get(Name, NtS, undefined) of
        undefined -> {error, not_found};
        #station{} = S -> {ok, S}
    end;

findStation({coord, Coord}, #monitor{coord_to_name = CtN, name_to_station = NtS}) ->
    case maps:get(Coord, CtN, undefined) of
        undefined -> {error, not_found};
        Name -> {ok, maps:get(Name, NtS)}
    end.

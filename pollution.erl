-module(pollution).
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyStationData/3, setup/0]).
-include_lib("eunit/include/eunit.hrl").

%---Utility functions---

containsStation(Monitor, Station) ->
    lists:any(fun(S) -> (maps:find(name, S) == maps:find(name, Station)) or (maps:find(coordinates, S) == maps:find(coordinates, Station)) end, Monitor).

containsMeasurement(Station, Measurement) ->
    lists:any(fun(M) -> (maps:find(date, M) == maps:find(date, Measurement)) and (maps:find(type, M) == maps:find(type, Measurement)) end, maps:get(measurements, Station)).

getStation({Latitude, Longitude}, Monitor) ->
    Coordinates = {Latitude, Longitude},
    case lists:dropwhile(fun(S) -> maps:get(coordinates, S) =/= Coordinates end, Monitor) of
        [] -> null;
        [H|_] -> H
    end;

getStation(Name, Monitor) ->
    case lists:dropwhile(fun(S) -> maps:get(name, S) =/= Name end, Monitor) of
        [] -> null;
        [H|_] -> H
    end.

getMeasurement(Date, Type, Station) ->
    case lists:dropwhile(fun(M) -> (maps:get(date, M) =/= Date) and (maps:get(type, M) =/= Type) end, maps:get(measurements, Station)) of
	    [] -> null;
	    [H|_] -> H
    end.

getHourAvg(List) -> getHourAvg(List, 0, [], (fun([]) -> 0; ([{H,_}|_]) -> H end)(List), 0).
getHourAvg([], Acc, ListAcc, Prev, Counter) -> [{Prev, Acc/Counter} | ListAcc];
getHourAvg([{H, V}|T], Acc, ListAcc, Prev, Counter) ->
    case Prev == H of
        true -> getHourAvg(T, Acc + V, ListAcc, H, Counter + 1);
        _ -> getHourAvg(T, V, [{Prev, Acc/Counter} | ListAcc], H, 1)
    end.

%---User interface functions---    

createMonitor() -> [].

addStation(Name, Coordinates, Monitor) ->
    case containsStation(Monitor, #{name => Name, coordinates => Coordinates, measurements => []}) of
        true -> {error, "Unable to modify monitor"};
        _ -> {ok, [#{name => Name, coordinates => Coordinates, measurements => []} | Monitor]}
    end.

addValue(Key, Date, Type, Value, Monitor) ->
    Station = getStation(Key, Monitor),
    case (Station == null) orelse containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        true -> {error, "Unable to modify monitor"};
        false -> {ok, [maps:update(measurements, [#{date => Date, type => Type, value => Value} | maps:get(measurements, Station)], Station) | lists:delete(Station, Monitor)]}
    end.

removeValue(Key, Date, Type, Monitor) ->
    Station = getStation(Key, Monitor),
    Measurement = getMeasurement(Date, Type, Station),
    case (Station == null) orelse (Measurement == null) of
        true -> {error, "Unable to modify monitor"};
        _ -> {ok, [maps:update(measurements, lists:delete(Measurement, maps:get(measurements, Station)), Station) | lists:delete(Station, Monitor)]}
    end.

getOneValue(Type, Date, Station, Monitor) -> 
    getMeasurement(Date, Type, getStation(Station, Monitor)).

getStationMean(Type, Station, Monitor) ->
    TypeList = lists:filter(fun(M) -> maps:get(type, M) == Type end, maps:get(measurements, getStation(Station, Monitor))),
    FinalList = lists:map(fun(M) -> maps:get(value, M) end, TypeList),
    lists:sum(FinalList)/length(FinalList).

getDailyMean(Type, Day, Monitor) ->
    [MeasurementList] = lists:map(fun(S) -> maps:get(measurements, S) end, Monitor),
    DateList = lists:filter(fun(M) -> {D,_} = maps:get(date, M), (maps:get(type, M) == Type) and (D == Day) end, MeasurementList),
    FinalList = lists:map(fun(M) -> maps:get(value, M) end, DateList),
    lists:sum(FinalList)/length(FinalList).

getHourlyStationData(Type, Name, Monitor) ->
    Station = getStation(Name, Monitor),
    TypeList = lists:filter(fun(M) -> maps:get(type, M) == Type end, maps:get(measurements, Station)),
    HourAndValueList = lists:map(fun(M) -> {_,{H,_,_}} = maps:get(date, M), {H, maps:get(value, M)} end, TypeList),
    getHourAvg(lists:keysort(1, HourAndValueList)).

%---Tests

containsStation_test() ->
    ?_assert(containsStation([], non_existing_station) == false).

containsMeasurement_test() ->
    P = addStation("Przybyszewskiego", {50.234, 20.31}, []),
    ?_assert(containsMeasurement(getStation("Przybyszewskiego", P), non_existing_measurement) == false).

getStation_test() ->
    ?_assert(getStation(non_existing_station, []) == null).

setup() ->
    P = pollution:createMonitor(),
    P1 = pollution:addStation("Przybyszewskiego", {50.3334, 19.3455}, P),
    P2 = pollution:addValue("Przybyszewskiego", {{2018,4,17},{12,20,40}}, "PM10", 59, P1),
    P3 = pollution:addValue("Przybyszewskiego", {{2018,4,17},{12,20,45}}, "PM10", 69, P2),
    P4 = pollution:addValue("Przybyszewskiego", {{2018,4,17},{12,20,50}}, "PM10", 79, P3), P4.
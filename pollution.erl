-module(pollution).
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyStationData/3]).

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
        true -> error;
        _ -> [#{name => Name, coordinates => Coordinates, measurements => []} | Monitor]
    end.

addValue({Latitude, Longitude}, Date, Type, Value, Monitor) ->
    Coordinates = {Latitude, Longitude},
    Station = getStation(Coordinates, Monitor),
    case (Station == null) orelse containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        true -> error;
        false -> [maps:update(measurements, [#{date => Date, type => Type, value => Value} | maps:get(measurements, Station)], Station) | lists:delete(Station, Monitor)]
    end;

addValue(Name, Date, Type, Value, Monitor) ->
    Station = getStation(Name, Monitor),
    case (Station == null) orelse containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        true -> error;
        false -> [maps:update(measurements, [#{date => Date, type => Type, value => Value} | maps:get(measurements, Station)], Station) | lists:delete(Station, Monitor)]
    end.

removeValue({Latitude, Longitude}, Date, Type, Monitor) ->
    Coordinates = {Latitude, Longitude},
    Station = getStation(Coordinates, Monitor),
    Measurement = getMeasurement(Date, Type, Station),
    case (Station == null) orelse (Measurement == null) of
        true -> error;
        _ -> [maps:update(measurements, lists:delete(Measurement, maps:get(measurements, Station)), Station) | lists:delete(Station, Monitor)]
    end;

removeValue(Name, Date, Type, Monitor) ->
    Station = getStation(Name, Monitor),
    Measurement = getMeasurement(Date, Type, Station),
    case (Station == null) orelse (Measurement == null) of
        true -> error;
        _ -> [maps:update(measurements, lists:delete(Measurement, maps:get(measurements, Station)), Station) | lists:delete(Station, Monitor)]
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
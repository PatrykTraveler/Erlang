-module(newpollution).
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

containsMeasurement(Station, Measurement) ->
    lists:any(fun(M) -> (maps:find(date, M) == maps:find(date, Measurement)) and (maps:find(type, M) == maps:find(type, Measurement)) end, maps:get(measurements, Station)).

containsStation(Monitor, Station) ->
    lists:any(fun(S) -> (maps:find(name, S) == maps:find(name, Station)) or (maps:find(coordinates, S) == maps:find(coordinates, Station)) end, maps:get(stations, Monitor)).

getStation({Latitude, Longitude}, Monitor) ->
    Coordinates = {Latitude, Longitude},
    [H|_] = lists:filter(fun(S) -> maps:get(coordinates, S) == Coordinates end, maps:get(stations, Monitor)), H;

getStation(Name, Monitor) ->
    [H|_] = lists:filter(fun(S) -> maps:get(name, S) == Name end, maps:get(stations, Monitor)), H.

getMeasurement(Date, Type, Station) ->
    [H|_] = lists:filter(fun(M) -> (maps:get(date, M) == Date) and (maps:get(type, M) == Type) end, maps:get(measurements, Station)), H.

createMonitor() ->
    #{stations => []}.

addStation(Name, Coordinates, Monitor) ->
    case containsStation(Monitor, #{name => Name, coordinates => Coordinates, measurements => []}) of
        true -> {error, contains_value};
        false -> maps:update(stations, [#{name => Name, coordinates => Coordinates, measurements => []} | maps:get(stations, Monitor)], Monitor)
    end.

addValue({Latitude, Longitude}, Date, Type, Value, Monitor) ->
    Coordinates = {Latitude, Longitude},
    Station = getStation(Coordinates, Monitor),
    case containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        true -> {error, contains_value};
        false -> maps:update(stations, [maps:update(measurements, [#{date => Date, type => Type, value => Value} | maps:get(measurements, Station)], Station) | lists:delete(Station, maps:get(stations, Monitor))], Monitor)
    end;

addValue(Name, Date, Type, Value, Monitor) ->
    Station = getStation(Name, Monitor),
    case containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        true -> {error, contains_value};
        false -> maps:update(stations, [maps:update(measurements, [#{date => Date, type => Type, value => Value} | maps:get(measurements, Station)], Station) | lists:delete(Station, maps:get(stations, Monitor))], Monitor)
    end.

removeValue({Latitude, Longitude}, Date, Type, Monitor) ->
    Coordinates = {Latitude, Longitude},
    Station = getStation(Coordinates, Monitor),
    Measurement = getMeasurement(Date, Type, Station),
    case containsMeasurement(Station, #{date => Date, type => Type, value => any_value}) of
        false -> {error, no_such_value};
        true -> maps:update(stations, [maps:update(measurements, lists:delete(Measurement, maps:get(measurements, Station)), Station)], Monitor)
    end;

removeValue(Name, Date, Type, Monitor) ->
    Station = getStation(Name, Monitor),
    Measurement = getMeasurement(Date, Type, Station),
    case containsMeasurement(Station, Measurement) of
        false -> {error, no_such_value};
        true -> maps:update(stations, [maps:update(measurements, lists:delete(Measurement, maps:get(measurements, Station)), Station) | lists:delete(Station, maps:get(stations, Monitor))], Monitor)
    end.

getOneValue(Type, Date, Station, Monitor) -> 
    getMeasurement(Date, Type, getStation(Station, Monitor)).

getStationMean(Type, Station, Monitor) ->
    TypeList = lists:filter(fun(M) -> maps:get(type, M) == Type end, maps:get(measurements, getStation(Station, Monitor))),
    FinalList = lists:map(fun(M) -> maps:get(value, M) end, TypeList),
    lists:sum(FinalList)/length(FinalList).

getDailyMean(Type, Day, Monitor) ->
    [MeasurementList] = lists:map(fun(S) -> maps:get(measurements, S) end, maps:get(stations, Monitor)),
    DateList = lists:filter(fun(M) -> {D,_} = maps:get(date, M), (maps:get(type, M) == Type) and (D == Day) end, MeasurementList),
    FinalList = lists:map(fun(M) -> maps:get(value, M) end, DateList),
    lists:sum(FinalList)/length(FinalList).

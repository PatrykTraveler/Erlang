-module(pollutionserver).
-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyStationData/3]).
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyStationData/2]).
-export([loop/1]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p~n", [Var]).
-endif.

%Request format: {request, Pid, {---function to perform---, [---List of args---]}}, where Pid is currently self().

start() ->
    io:format("My PID is: ~w~n", [self()]),
    register(pollutionServer, spawn(?MODULE, loop, [pollution:createMonitor()])).

stop() ->
    pollutionServer ! stop.

loop(Monitor) ->
    receive
        stop -> ok;
        {request, Pid, {add_station, Args}} ->
            Response = pollution:addStation(lists:nth(1, Args), lists:nth(2, Args), Monitor),
            case Response of
                {error, Err} -> Pid ! {reply, Err}, loop(Monitor);
                {ok, Value} ->  Pid ! {reply, ok}, loop(Value)
            end;
        {request, Pid, {add_value, Args}} ->
            Response = pollution:addValue(lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args), lists:nth(4, Args), Monitor),
            case Response of   
                {error, Err} -> Pid ! {reply, Err}, loop(Monitor);
                {ok, Value} -> Pid ! {reply, ok}, loop(Value)
            end;
        {request, Pid, {remove_value, Args}} ->
            Response = pollution:removeValue(lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args), Monitor),
            case Response of
                {error, Err} -> Pid ! {reply, Err}, loop(Monitor);
                {ok, Value} -> Pid ! {reply, ok}, loop(Value)
            end;
        {request, Pid, {get_one_value, Args}} ->
            Response = pollution:getOneValue(lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args), Monitor),
            case Response of
                {error, Err} -> Pid ! {reply, Err}, loop(Monitor);
                {ok, Value} -> Pid ! {reply, ok}, loop(Value)
            end;
        {request, Pid, {get_station_mean, Args}} ->
            Response = pollution:getStationMean(lists:nth(1, Args), lists:nth(2, Args), Monitor),
            case Response of
                {error, Err} -> Pid ! {reply, Err}, loop(Monitor);
                {ok, Value} ->  Pid ! {reply, ok}, loop(Value)
            end;
        {request, Pid, print_monitor} ->
            Pid ! {reply, ok}, io:format("~w~n", [Monitor]), loop(Monitor);
        _ -> loop(Monitor)
    end.

addStation(Name, Coordinates) ->
    pollutionServer ! {request, self(), {add_station, [Name, Coordinates]}}.

addValue(Key, Date, Type, Value) ->
    pollutionServer ! {request, self(), {add_value, [Key, Date, Type, Value]}}.

removeValue(Key, Date, Type) ->
    pollutionServer ! {request, self(), {remove_value, [Key, Date, Type]}}.

getOneValue(Type, Date, Station) ->
    pollutionServer ! {request, self(), {get_one_value, [Type, Date, Station]}}.

getStationMean(Type, Station) ->
    pollutionServer ! {request, self(), {get_station_mean, [Type, Station]}}.

getDailyMean(Type, Day) ->
    pollutionServer ! {request, self(), {get_daily_mean, [Type, Day]}}.

getHourlyStationData(Type, Name) ->
    pollutionServer ! {request, self(), {get_hourly_station_data, [Type, Name]}}.
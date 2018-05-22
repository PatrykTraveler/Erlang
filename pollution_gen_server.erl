-module(pollution_gen_server).
-behaviour(gen_server).
-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyStationData/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/1, stop/0, getOneValue/3, addValue/4, addStation/2, removeValue/3]).

start_link(InitialValue) ->
    gen_server:start_link({local, pollution_gen_server}, ?MODULE, InitialValue, []).

stop() -> gen_server:cast(pollution_gen_server, {stop}).

getOneValue(Type, Date, Station) ->
    gen_server:call(pollution_gen_server, {get_one_value, Type, Date, Station}).

getStationMean(Type, Station) ->
    gen_server:call(pollution_gen_server, {get_station_mean, Type, Station}).

getDailyMean(Type, Day) ->
    gen_server:call(pollution_gen_server, {get_daily_mean, Type, Day}).

getHourlyStationData(Type, Name) ->
    gen_server:call(pollution_gen_server, {get_hourly_station_data, Type, Name}).

addStation(Name, Coordinates) ->
    gen_server:cast(pollution_gen_server, {add_station, Name, Coordinates}).

addValue(Key, Date, Type, Value) ->
    gen_server:cast(pollution_gen_server, {add_value, Key, Date, Type, Value}).
    
removeValue(Key, Date, Type) ->
    gen_server:cast(pollution_gen_server,{remove_value, Key, Date, Type}).

%% SERVER    

init(_) -> {ok, pollution:createMonitor()}.

handle_call({get_one_value, Type, Data, Station}, _, LoopData) ->
    Value = pollution:getOneValue(Type, Data, Station, LoopData),
    {reply, Value, LoopData};

handle_call({get_station_mean, Type, Station}, _, LoopData) ->
    Value = pollution:getStationMean(Type, Station, LoopData),
    {reply, Value, LoopData};

handle_call({get_daily_mean, Type, Day}, _, LoopData) ->
    Value = pollution:getDailyMean(Type, Day, LoopData),
    {reply, Value, LoopData};

handle_call({get_hourly_station_data, Type, Day}, _, LoopData) ->
    Value = pollution:getHourlyStationData(Type, Day, LoopData),
    {reply, Value, LoopData}.

handle_cast({stop}, LoopData) -> {stop, normal, LoopData};

handle_cast({add_station, Name, Coordinates}, LoopData) ->
    Value = pollution:addStation(Name, Coordinates, LoopData),
    case Value of
        {error, _} -> {reply, Value, LoopData};
        {ok, Monitor} -> {reply, ok, Monitor}
    end;

handle_cast({add_value, Key, Date, Type, Value}, LoopData) ->
    Value = pollution:addValue(Key, Date, Type, Value, LoopData),
    case Value of
        {error, _} -> {reply, Value, LoopData};
        {ok, Monitor} -> {reply, ok, Monitor}
    end;

handle_cast({remove_value, Key, Date, Type}, LoopData) ->
    Value = pollution:removeValue(Key, Date, Type, LoopData),
    case Value of
        {error, _} -> {reply, Value, LoopData};
        {ok, Monitor} -> {reply, ok, Monitor}
    end.

terminate() -> gen_server:stop(pollution_gen_server).





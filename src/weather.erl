-module(weather).
-export([forecast/1]).


%% Synchronize way to get the City Report taking more than 2 seconds. so that i have use async way to collect the information.
%% one more major important is the order of result.

forecast(Cities) ->
  TotalCount = lists:foldr(fun(City,Count) -> worker(self(),City),Count+1 end,0,Cities),
  loop(Cities,TotalCount,[]).

loop(Cities,0,Result) -> return_result(Cities,Result);
loop(Cities,N,Result) ->
  receive {City,Status} ->  loop(Cities,N-1,[{City,Status}|Result])  end.

return_result([],_) -> [];
return_result([City|Rest],ResultSet) -> [proplists:get_value(City,ResultSet)] ++ return_result(Rest,ResultSet).


worker(Root,City) -> spawn(fun() ->  erlang:send(Root,{City,find_in(weather_api:get_weather(City))})  end).

find_in({weather,_current,{forecast,_,Status}}) -> Status;
find_in(Other) ->  Other.



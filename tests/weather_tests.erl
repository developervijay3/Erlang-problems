-module(weather_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

% The api module must simulate time spent in an API call.
% Don't remove the sleep!
api_test() ->
  {Time, _} = timer:tc(weather_api, get_weather, [foo]),
  ?assert(Time >= 500000).

forecast_test() ->
  {Time, Value} = timer:tc(weather, forecast, [["Chicago", "Sydney", "London", "San Antonio", "Antarctica"]]),
  % We got the right data
  ?assertEqual(["Windy", "Clear", "Rainy", "Hot", "Cold"], Value),
  % And it took less than 2 seconds
  io:format("~p",[Time]),
  ?assert(Time < 1 * 1000 * 1000).

forecast_wrong_data_test()->
  % searching with invalid city name or city which is not having weather data
  Value = weather:forecast(["Chicago", "Sydney", "London", "any", "Antarctica"]),
  ?assertEqual(["Windy", "Clear", "Rainy", undefined, "Cold"], Value).


-module(json).
-export([render/1]).

%% Json can start with either JSON Object or JSON Array.

%% for a clarity, let consider a json array of integer like [33,222,] to make it this by erlang term like [{33},{222}].

%% In this solution I have not used Accumulator. I have already completed the same json encoder with Accumulator you can get that from my github account(regupathy)

%% this approach only good for small size of json. because it cause the stack overload for big size data.

render([Tuple|_] = Data) when is_tuple(Tuple)-> jsonObjectSet(Data);
render(Tuple) when is_tuple(Tuple) -> jsonObjectSet(Tuple);
render(JSONArray) -> jsonArraySet(JSONArray).

jsonObjectSet({Data}) -> [ ${ | jsonObject(Data) ++ [$}]].

jsonObject([]) ->  [];
jsonObject({_Key,_Value} = Pair) -> jsonElement(Pair);
jsonObject([{_Key,_Value} = Pair|[]]) -> jsonElement(Pair);
jsonObject([{_Key,_Value} = Pair|Rest]) -> jsonElement(Pair) ++ [$,|jsonObject(Rest)].

jsonArraySet(Data) -> [$[|jsonArray(Data) ++ [$]]].

jsonArray([[]]) -> [[]];
jsonArray([]) -> [];
jsonArray([Item|[]]) -> valueConform(Item);
jsonArray([Item|Rest]) -> valueConform(Item) ++ [$,|jsonArray(Rest)].


jsonElement({JsonKey,JsonValue}) -> json_key(JsonKey) ++ [ $: | valueConform(JsonValue)].


%% All Json object key should be a string

json_key(Atom)when is_atom(Atom) -> json_key(atom_to_list(Atom));
json_key(Integer) when is_integer(Integer) -> json_key(integer_to_list(Integer));
json_key(Binary) when is_binary(Binary) -> json_key(binary_to_list(Binary));
json_key(String) ->  conform_double_quote(String).


valueConform({[_|_]}=Data)  -> jsonObjectSet(Data);
valueConform({Value})  -> json_value(Value);
valueConform(Tuple) when is_tuple(Tuple) -> jsonObjectSet(Tuple);
valueConform([Tuple|_] = Data)when is_tuple(Tuple) -> jsonArraySet(Data);
valueConform([Binary|_] = Data)when is_binary(Binary) -> jsonArraySet(Data);
valueConform([[_|_]|_] = Data) -> jsonArraySet(Data);
valueConform(Rest) -> json_value(Rest).


%% Json Object value can be in String | integer | boolean

json_value(Integer) when is_integer(Integer) -> integer_to_list(Integer);
json_value(Float) when is_float(Float) -> float_to_list(Float);
json_value(true) -> atom_to_list(true);
json_value(false) -> atom_to_list(false);
json_value(Atom) when is_atom(Atom) -> conform_double_quote(atom_to_list(Atom));
json_value(Binary) when is_binary(Binary) -> conform_double_quote(binary_to_list(Binary));
json_value(String) -> conform_double_quote(String).

conform_double_quote([$"|_] = String) -> String;
conform_double_quote(String)  -> [$"| String ++ [$"]].


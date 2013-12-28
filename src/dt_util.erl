-module(dt_util).
-export([timestamp/0, hexstring/1]).

%% Return unix style timestamp, seconds since epoch
timestamp() ->
	{M, S, _} = os:timestamp(),
	(M*1000000) + S.

hexstring(Data) when is_binary(Data) ->
	lists:flatten(lists:map(
		fun(X) -> io_lib:format("~2.16.0b", [X]) end,
		binary_to_list(Data))).
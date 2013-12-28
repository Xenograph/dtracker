-module(dt_request_parser).
-export([parse/1]).

parse(RequestStr) ->
	case catch parse_internal(RequestStr) of
		{'EXIT', _} -> {error, unparsed};
		Parsed -> Parsed
	end.

parse_internal(RequestStr) ->
	[PassKey, Request] = string:tokens(string:substr(RequestStr, 2), "/"),
	[Action, RequestData] = string:tokens(Request, "?"),
	KVPairs = string:tokens(RequestData, "&"),
	DataDict =lists:foldl(
		fun(KVPair, Acc) ->
			[Key, Value] = string:tokens(KVPair, "="),
			orddict:store(Key, Value, Acc)
		end,
		orddict:new(),
		KVPairs),
	{PassKey, string:to_lower(Action), DataDict}.
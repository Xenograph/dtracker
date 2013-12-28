-module(dt_bencode).
-export([encode/1, decode/1]).

%% API
encode(Data) ->
	encode_internal(Data).

decode(Data) ->
	case catch decode_internal(Data) of
		{'EXIT', _} -> {error, unparsed};
		{Decoded, _} -> {ok, Decoded}
	end.

encode_internal(Integer) when is_integer(Integer) ->
	BinaryInt = integer_to_binary(Integer),
	<<"i", BinaryInt/binary, "e">>;

%% Encoding
encode_internal(BinString) when is_binary(BinString) ->
	BinarySize = integer_to_binary(size(BinString)),
	<<BinarySize/binary, ":", BinString/binary>>;

encode_internal(String) when is_list(String) ->
	encode_internal(list_to_binary(String));

encode_internal({list, List}) when is_list(List) ->
	EncodedList = [encode(E) || E <- List],
	BinaryList = list_to_binary(EncodedList),
	<<"l", BinaryList/binary, "e">>;

encode_internal({dict, Dict}) ->
	KVList = orddict:to_list(Dict),
	BinaryList = list_to_binary(lists:map(fun({Key, Val}) -> [encode(Key), encode(Val)] end, KVList)),
	<<"d", BinaryList/binary, "e">>.

%% Decoding
decode_internal(<<$i, Data/binary>>) ->
	decode_integer(Data, []);
decode_internal(<<$l, Data/binary>>) ->
	decode_list(Data, []);
decode_internal(<<$d, Data/binary>>) ->
	decode_dict(Data, orddict:new());
decode_internal(Data) ->
	decode_string(Data, []).

decode_integer(<<$e, Extra/binary>>, Acc) ->
	{list_to_integer(Acc),Extra};
decode_integer(<<C, Rest/binary>>, Acc) ->
	decode_integer(Rest, Acc++[C]).

decode_string(<<$:, Rest/binary>>, Acc) ->
	Len = list_to_integer(Acc),
	<<String:Len/binary, Extra/binary>> = Rest,
	{String, Extra};
decode_string(<<C, Rest/binary>>, Acc) ->
	decode_string(Rest, Acc++[C]).

decode_list(<<$e, Extra/binary>>, Acc) ->
	{{list, Acc}, Extra};
decode_list(Data, Acc) ->
	{Dec, Rest} = decode_internal(Data),
	decode_list(Rest, Acc++[Dec]).

decode_dict(<<$e, Extra/binary>>, Acc) ->
	{{dict, Acc}, Extra};
decode_dict(Data, Acc) ->
	{Dec, Rest} = decode_internal(Data),
	decode_dict(Rest, Acc, Dec).

decode_dict(Data, Acc, Key) ->
	{Dec, Rest} = decode_internal(Data),
	decode_dict(Rest, orddict:store(Key, Dec, Acc)).
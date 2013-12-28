-module(dt_passkey).
-export([generate_passkey/0]).

generate_passkey() ->
	dt_util:hexstring(crypto:strong_rand_bytes(16)).
-module(dtracker).
-behavior(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	dtracker_sup:start_link().

stop(_State) ->
	ok.
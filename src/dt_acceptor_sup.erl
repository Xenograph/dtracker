-module(dt_acceptor_sup).
-behaviour(supervisor).
-export([init/1, start_link/1]).
-define(NUM_ACCEPTORS, 20).

init(ListenSocket) ->
	spawn_link(fun start_listeners/0),
	{
		ok,
		{
			{
				simple_one_for_one, %% Restart strategy
				5,                  %% Max Restarts
				60                  %% Max Time
			},
			[
				{
					dt_acceptor_id, %% ID
					{
						dt_acceptor,   %% Module Name
						start_link,    %% Function Name
						[ListenSocket] %% Args
					},
					permanent,    %% Restart Type
					5000,         %% Max shutdown wait time (5 sec)
					worker,       %% Child Type
					[dt_acceptor] %% Callback Module
				}
			]
		}
	}.

start_link(ListenSocket) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, ListenSocket).

addSocket() ->
	supervisor:start_child(?MODULE, []).

start_listeners() ->
	io:format("Starting ~B acceptor workers~n", [?NUM_ACCEPTORS]),
	[addSocket() || _ <- lists:seq(1,?NUM_ACCEPTORS)],
	ok.
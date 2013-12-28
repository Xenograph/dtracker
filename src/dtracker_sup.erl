-module(dtracker_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

init(_Arg) ->
	{
		ok,
		{
			{
				one_for_all, %% Restart strategy
				5,           %% Max Restarts
				60           %% Max Time
			},
			[
				{
					dt_database_sup_id, %% ID
					{
						dt_database_sup, %% Module Name
						start_link, %% Function Name
						[ [ {"127.0.0.1", 10017},{"127.0.0.1", 10027},{"127.0.0.1", 10037},{"127.0.0.1", 10047},{"127.0.0.1", 10057} ] ]          %% Args
					},
					permanent,   %% Restart Type
					10000,       %% Max shutdown wait time (10 sec)
					supervisor,  %% Child Type
					[dt_database_sup] %% Callback Module
				},
				{
					client_sup_id, %% ID
					{
						dt_client_sup, %% Module Name
						start_link, %% Function Name
						[]          %% Args
					},
					permanent,   %% Restart Type
					10000,       %% Max shutdown wait time (10 sec)
					supervisor,  %% Child Type
					[dt_client_sup] %% Callback Module
				}
			]
		}
	}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
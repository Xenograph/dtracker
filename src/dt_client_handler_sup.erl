-module(dt_client_handler_sup).
-behaviour(supervisor).
-export([init/1, start_link/0, handleNewConnection/1]).

init(_Args) ->
	{
		ok,
		{
			{
				simple_one_for_one,
				5,
				60
			},
			[
				{
					dt_handler_id, %% ID
					{
						dt_client_handler,   %% Module Name
						start_link,    %% Function Name
						[] %% Args
					},
					temporary,    %% Restart Type
					5000,         %% Max shutdown wait time (5 sec)
					worker,       %% Child Type
					[dt_client_handler] %% Callback Module
				}
			]
		}
	}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

handleNewConnection(Socket) ->
	supervisor:start_child(?MODULE, [Socket]).
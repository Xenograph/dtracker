-module(dt_client_sup).
-behaviour(supervisor).
-export([init/1, start_link/0, startAcceptorSup/1]).

init(_Args) ->
	{
		ok,
		{
			{
				rest_for_one, %% Restart strategy
				5,                  %% Max Restarts
				60                  %% Max Time
			},
			[
				{
					dt_listener_id, %% ID
					{
						dt_listener,   %% Module Name
						start_link,    %% Function Name
						[12345] %% Args
					},
					permanent,    %% Restart Type
					5000,         %% Max shutdown wait time (2 sec)
					worker,       %% Child Type
					[dt_listener] %% Callback Module
				},
				{
					dt_handlerSup_id,
					{
						dt_client_handler_sup,
						start_link,
						[]
					},
					permanent,
					5000,
					supervisor,
					[dt_client_handler_sup]
				}
			]
		}
	}.


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

startAcceptorSup(ListenSocket) ->
	% Start the acceptor supervisor with given ListenSocket
	supervisor:start_child(?MODULE,
	{
		dt_acceptorSup_id, %% ID
		{
			dt_acceptor_sup,   %% Module Name
			start_link,    %% Function Name
			[ListenSocket] %% Args
		},
		permanent,    %% Restart Type
		2000,         %% Max shutdown wait time (5 sec)
		supervisor,       %% Child Type
		[dt_acceptor_sup] %% Callback Module
	}).
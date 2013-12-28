-module(dt_database_sup).
-behaviour(supervisor).
-export([init/1, start_link/1]).

init(NodeList) ->
	{
		ok,
		{
			{
				one_for_all, %% Restart strategy
				5,                  %% Max Restarts
				60                  %% Max Time
			},
			[
				{
					dt_db_conn_sup_id, %% ID
					{
						dt_db_conn_sup,   %% Module Name
						start_link,    %% Function Name
						[] %% Args
					},
					permanent,    %% Restart Type
					5000,         %% Max shutdown wait time (2 sec)
					supervisor,       %% Child Type
					[dt_db_conn_sup] %% Callback Module
				},
				{
					dt_database_id,
					{
						dt_database,
						start_link,
						[NodeList]
					},
					permanent,
					5000,
					worker,
					[dt_database]
				},
				{
					dt_down_dbnode_monitor_id,
					{
						dt_down_dbnode_monitor,
						start_link,
						[]
					},
					permanent,
					5000,
					worker,
					[dt_down_dbnode_monitor]
				}
			]
		}
	}.

start_link(NodeList) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, NodeList).
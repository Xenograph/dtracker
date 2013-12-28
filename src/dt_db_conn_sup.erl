-module(dt_db_conn_sup).
-behaviour(supervisor).

-export([init/1, start_link/0, createConnection/1]).

init(_Arg) ->
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
					riak_pb_socket_id, %% ID
					{
						riakc_pb_socket,   %% Module Name
						start_link,    %% Function Name
						[] %% Args
					},
					temporary,    %% Restart Type
					5000,         %% Max shutdown wait time (5 sec)
					worker,       %% Child Type
					[riakc_pb_socket] %% Callback Module
				}
			]
		}
	}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

createConnection(_Node = {Host, Port}) ->
	io:format("Starting DB Socket~n"),
	io:format("Num DB Sockets: ~.10B~n", [proplists:get_value(active, supervisor:count_children(?MODULE))+1]),
	supervisor:start_child(?MODULE, [Host, Port]).
-module(dt_down_dbnode_monitor).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(INTERVAL, 60000).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Arg) ->
	erlang:send_after(?INTERVAL, self(), check_nodes),
	{ok, undefined}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(check_nodes, State) ->
	NodeList = dt_database:get_down_nodes(),
	lists:foreach(fun check_node/1, NodeList),
	erlang:send_after(?INTERVAL, self(), check_nodes),
	{noreply, State}.

terminate(_Reason, _Sock) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

check_node(Node = {Host, Port}) ->
	case riakc_pb_socket:start_link(Host, Port) of
		{ok, SockPid} ->
			case catch riakc_pb_socket:ping(SockPid, 5000) of
				pong ->
					dt_database:notify_node_up(Node),
					catch riakc_pb_socket:stop(SockPid),
					up;
				_ ->
					catch riakc_pb_socket:stop(SockPid),
					still_down
			end;
		{error, _} ->
			still_down
	end.
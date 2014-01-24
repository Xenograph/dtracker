-module(dt_database).
-behaviour(gen_server).
-export([start_link/1, execute/1, get_down_nodes/0, notify_node_up/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {nodes, down_nodes, pids}).
-define(MAX_CONNECT_ATTEMPTS, 10).

%% Interface
start_link(NodeList) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, NodeList, []).

execute(Func) ->
	case gen_server:call(?MODULE, check_out) of
		{ok, Pid} ->
			try {ok, Func(Pid)}
			catch _:Err -> {error, Err}
			after gen_server:cast(?MODULE, {check_in, Pid}) end;
		{error, Err} ->
			{error, Err}
	end.

get_down_nodes() ->
	gen_server:call(?MODULE, get_down_nodes).

notify_node_up(Node) ->
	gen_server:cast(?MODULE, {node_up, Node}).

%% gen_server internals
init(NodeList) when is_list(NodeList)->
	{ok, #state{nodes=NodeList, down_nodes=[], pids=queue:new()}}.

handle_call(check_out, _From, State = #state{nodes=NodeList, down_nodes=DownList, pids=PidQueue}) ->
	case get_available_socket(NodeList, DownList, PidQueue) of
		{ok, Pid, NewPidQueue, NewNodeList, NewDownList} ->
			{reply, {ok, Pid}, State#state{pids=NewPidQueue, nodes=NewNodeList, down_nodes=NewDownList}};
		{{error, Error}, NewNodeList, NewDownList} ->
				{reply, {error, Error}, State#state{nodes=NewNodeList, down_nodes=NewDownList}}
	end;
handle_call(get_down_nodes, _From, State = #state{down_nodes=DownList}) ->
	{reply, DownList, State}.

handle_cast({check_in, Pid}, State = #state{pids=PidQueue}) ->
	{noreply, State#state{pids=queue:in(Pid, PidQueue)}};

handle_cast({node_up, Node}, State = #state{nodes=NodeList, down_nodes=DownList}) ->
	{NewNodeList, NewDownList} = set_node_up(Node, NodeList, DownList),
	{noreply, State#state{nodes=NewNodeList, down_nodes=NewDownList}}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _Sock) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Helper Functions
pick_node(NodeList) when is_list(NodeList) ->
	lists:nth(random:uniform(length(NodeList)), NodeList). %% Pick a random node from the given nodelist

set_node_down(Node, NodeList, DownList) ->
	case (not lists:member(Node, DownList)) and lists:member(Node, NodeList) of
		false -> {NodeList, DownList};
		true ->
			{
				lists:delete(Node, NodeList),
				DownList++[Node]
			}
	end.

set_node_up(Node, NodeList, DownList) ->
	case (not lists:member(Node, NodeList)) and lists:member(Node, DownList) of
		false -> {NodeList, DownList};
		true ->
			{
				NodeList++[Node],
				lists:delete(Node, DownList)
			}
	end.

get_available_socket(NodeList, DownList, PidQueue) ->
	case queue:out(PidQueue) of
		{{value, PidOut}, NewPidQueue} ->
			case is_process_alive(PidOut) of
				true -> {ok, PidOut, NewPidQueue, NodeList, DownList};
				false -> get_available_socket(NodeList, DownList, NewPidQueue)
			end;
		{empty, _} ->
			case new_socket(NodeList, DownList) of
				{ok, NewNodeList, NewDownList, Pid} ->
					{ok, Pid, PidQueue, NewNodeList, NewDownList};
				{{error, Error}, NewNodeList, NewDownList} ->
					{{error, Error}, NewNodeList, NewDownList}
			end
	end.

new_socket(NodeList, DownList) ->
	new_socket(NodeList, DownList, 1).
new_socket(NodeList, DownList, ?MAX_CONNECT_ATTEMPTS) ->
	{{error, could_not_create_connection}, NodeList, DownList};
new_socket(NodeList, DownList, AttemptNum) ->
	PickedNode = pick_node(NodeList),
	case dt_db_conn_sup:createConnection(PickedNode) of
		{ok, Pid} ->
			{ok, NodeList, DownList, Pid};
		{error, _} ->
			{NewNodeList, NewDownList} = set_node_down(PickedNode, NodeList, DownList),
			new_socket(NewNodeList, NewDownList, AttemptNum+1)
	end.
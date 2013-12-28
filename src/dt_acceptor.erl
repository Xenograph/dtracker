-module(dt_acceptor).
-behaviour(gen_server).
-export([start_link/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% Start (linked) dt_acceptor with given listen socket
start_link(ListenSocket) ->
	gen_server:start_link(?MODULE, ListenSocket, []).

%% Initialize dt_acceptor
init(ListenSocket) ->
	gen_server:cast(self(), accept),
	{ok, ListenSocket}.

%% Wait for incoming client connection
handle_cast(accept, Sock) ->
	case gen_tcp:accept(Sock) of
		{ok, AcceptSocket} ->
			dt_client_handler_sup:handleNewConnection(AcceptSocket),
			gen_server:cast(self(), accept),
			{noreply, Sock};
		_ ->
			{stop, accept_error, Sock}
	end.

%% Tidy things up
terminate(_Reason, _Sock) ->
	ok.

%% Currently Unused
handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
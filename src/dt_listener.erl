-module(dt_listener).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Interface
start_link(ListenPort) ->
	gen_server:start_link(?MODULE, ListenPort, []).

%% gen_server internals
init(ListenPort) ->
	process_flag(trap_exit, true),
	io:format("Opening Listen Socket...~n"),
	case gen_tcp:listen(ListenPort, [binary, {packet, http}, {active, false}]) of
		{ok, Socket} ->
			gen_server:cast(self(), startAcceptorSup),
			{ok, Socket};
		_ ->
			io:format("Failed to bind to port!~n"),
			{stop, error_creating_listen_socket}
	end.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(startAcceptorSup, Socket) ->
	dt_client_sup:startAcceptorSup(Socket),
	{noreply, Socket}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, Socket) ->
	io:format("Closing Listen Socket...~n"),
	gen_tcp:close(Socket),
	io:format("Listen socket closed!~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
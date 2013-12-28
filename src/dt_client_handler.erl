-module(dt_client_handler).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, uri}).

-define(REQUEST_TIMEOUT, 5000).

%% Interface
start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).


%% gen_server internals
init(Socket) ->
	gen_server:cast(self(), wait_for_request),
	{ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(wait_for_request, State = #state{socket=Sock}) ->
	case gen_tcp:recv(Sock, 0, ?REQUEST_TIMEOUT) of
		{ok, {http_request, 'GET', {abs_path, URI}, _}} ->
			%% Valid Request, send to handler
			handle_request(Sock, URI),
			{stop, normal, State};
		{ok, http_request, _, _, _} ->
			gen_tcp:send(Sock, http_response(error_response_code(100))),
			{stop, normal, State};
		_ ->
			{stop, normal, State}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{socket=Sock}) ->
	gen_tcp:close(Sock),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Helper functions
handle_request(Sock, URI) ->
	io:format("Receving GET Request with URI: ~s~n", [URI]),
	case dt_request_parser:parse(URI) of
		{error, unparsed} ->
			gen_tcp:send(Sock, http_response(error_response("Malformed Request")));
		{Passkey, Action, RequestData} ->
			{ok, {IPAddress, _}} = inet:peername(Sock),
			gen_tcp:send(Sock, http_response(request_response(Action, Passkey, IPAddress, RequestData)))
	end.

request_response("announce", Passkey, OriginIP, RequestData) ->
	AnnouncedIP = OriginIP,
	Event = default,
	NumWant = 50,
	catch AnnouncedIP = orddict:fetch("ip", RequestData),
	catch Event = orddict:fetch("event", RequestData),
	catch NumWant = orddict:fetch("numwant", RequestData),
	case catch announce_response(Passkey, OriginIP, AnnouncedIP, orddict:fetch("info_hash", RequestData),
								orddict:fetch("peer_id", RequestData), orddict:fetch("port", RequestData),
								orddict:fetch("uploaded", RequestData), orddict:fetch("downloaded", RequestData),
								orddict:fetch("left", RequestData), Event, NumWant) of
		{'EXIT', _} -> error_response("Invalid Announce Request");
		Response -> Response
	end;

request_response(_Action, _Passkey, _IPAddress, _RequestData) ->
	error_response("Invalid Action").

announce_response(_Passkey, _OriginIP, _AnnouncedIP, _InfoHash, _PeerID, _Port, _Uploaded, _Downloaded, _Left, _Event, _NumWant) ->
	error_response("Tracker not written").

%% Return a BEncode-formatted error response with given reason
error_response(Reason) ->
	ResponseDict = orddict:store("failure reason", Reason, orddict:new()),
	dt_bencode:encode({dict, ResponseDict}).

%% Return a BEncode-formatted error response with given reason code
error_response_code(Code) ->
	ResponseDict = orddict:store("failure code", Code, orddict:new()),
	dt_bencode:encode({dict, ResponseDict}).

%% Return an HTTP GET response with given data
http_response(Data) when is_binary(Data) ->
	iolist_to_binary(
		io_lib:fwrite(
			"HTTP/1.1 200 OK\nContent-Type: text/plain\nContent-Length: ~p\n\n~s",
			[size(Data), Data])).
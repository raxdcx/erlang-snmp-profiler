-module(snmp_profiler_stats).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-import(snmp_profiler_utils, [die/2, log/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

init(#{}) ->
    ets:new(snmp_profiler_stats, [named_table, public]),
    {ok, #{}}.

%% @doc Lets the main program wait for all metrics to be sent before shutting down.
%% Since this won't be called until the end of the test, it should be the last 
%% message in the queue. It's just up to the caller to wait long enough, and
%% then the app can shut down. No need to stop here. That would just force me
%% to implement a terminate() function.
handle_call(await, _From, _State) ->
    {reply, ok, normal}.

handle_cast(Request, State) ->
    {SimpleName, Value} = case Request of
			      {histo, Name, V} when is_float(V) -> {Name, float_to_binary(V)};
			      {histo, Name, V} when is_integer(V) -> {Name, integer_to_binary(V)};
			      {count, Name} ->
				  Count = ets:update_counter(snmp_profiler_stats, Name, 1, {Name, 0}),
				  {Name, integer_to_binary(Count)}
			  end,
    MetricName = list_to_binary(["snmp_profiler.", SimpleName]),
    Time = integer_to_binary(erlang:system_time(seconds)),
    NewState = send_or_retry(MetricName, Value, Time, State),
    {noreply, NewState#{can_stop => false}}.

send_or_retry(MetricName, Value, Time, State) ->
    {Socket, State1} = get_live_socket(State),
    Packet = <<MetricName/binary, " ", Value/binary, " ", Time/binary, "\n">>,
    case gen_tcp:send(Socket, Packet) of
	ok -> State1;
	Error ->
	    log(error, "Failed to send metric to graphite. Reconnecting and retrying. Error: ~p", [Error]),
	    timer:sleep(1000),
	    NewSocket = connect_until_connected(),
	    State2 = State1#{socket := NewSocket},
	    send_or_retry(MetricName, Value, Time, State2)
	end.

get_live_socket(#{socket := Socket} = State) ->
    {Socket, State};
get_live_socket(State) ->
    S = connect_until_connected(),
    {S, State#{socket => S}}.

connect_until_connected() ->
    case gen_tcp:connect("graphite", 2003, []) of
	{ok, Socket} ->
	    Socket;
	Error ->
	    log(error, "Failed to connect to graphite. Reconnecting and retrying. Error: ~p", [Error]),
	    timer:sleep(1000),
	    connect_until_connected()
    end.

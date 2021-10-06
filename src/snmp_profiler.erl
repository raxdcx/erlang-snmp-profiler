-module(snmp_profiler).
-export([profile/0, profile/1]).
-import(snmp_profiler_utils, [die/2, log/3]).

profile() ->
     profile([]).

profile(Args) ->
    {ok, _} = snmp_profiler_stats:start_link(),
    ok = snmp_profiler_config:parse_input(Args),
    log(debug, "Profiling with config: ~p", [snmp_profiler_config:dump()]),
    ok = start_snmp(),
    run_switch(snmp_profiler_config:test_one()),
    log(info, "Waiting for all metrics to be sent...", []),
    try
	gen_server:call(snmp_profiler_stats, await, timer:minutes(1))
    catch
	exit:{timeout, _} ->
	    log(error, "Timed out waiting for metrics to finish sending. Stopping without sending all metrics. Graphs may be incomplete.", [])
    end.
    
start_snmp() ->
    application:load(snmp),
    ManagerConfig = [{config, [
			{dir, "snmp_work_dir/config/"},
			{db_dir, "snmp_work_dir/db/"}]}],
    application:set_env(snmp, manager, ManagerConfig),
    case application:ensure_all_started(snmp) of
	{ok, Started} -> 
	    log(debug, "Started applications: ~p", [Started]),
	    ok;
	Error1 ->
	    Msg1 = io_lib:format("Failed to start Erlang SNMP:\n~p", [Error1]),
	    die(Msg1, [internal])
    end,
    case snmpm:register_user("snmp_profiler", snmpm_user_default, undefined) of
	ok -> 
	    log(debug, "Registered SNMP user", []),
	    ok;
	Error2 ->
	    Msg2 = io_lib:format("Failed to register SNMP user:\n~p", [Error2]),
	    die(Msg2, [internal])
    end.

run_switch(Name) ->
    log(info, "Testing switch ~s", [Name]),
    FullName = case re:run(Name, "\.rackspace\.net$", [{capture, none}]) of
		   match -> Name;
		   nomatch -> Name ++ ".rackspace.net"
	       end,
    case inet:getaddr(FullName, inet) of
	{error, nxdomain} ->
	    log(user, "The switch name '~s' doesn't resolve in DNS. Skipping it.", [Name]),
	    bad_switch_name;
	{ok, Address} ->
	    log(debug, "Address for ~s is ~p", [FullName, Address]),
	    run_switch_with_addr(FullName, Address)
    end.

run_switch_with_addr(FullName, Address) ->
    Oid = oid_to_walk(),
    Opts = [{engine_id, "manager's engine"},
	    {address,   Address},
	    {community, snmp_profiler_config:community_string()},
	    {version,   v2},
	    {sec_model, v2c}],
    UserId = "snmp_profiler",
    log(debug, "Register SNMP agent for ~s", [FullName]),
    case snmpm:register_agent(UserId, FullName, Opts) of
	ok -> ok;
	Error ->
	    Msg = io_lib:format("Failed to register SNMP agent:\n~p", [Error]),
	    die(Msg, [internal])
    end,
    Result = walk(UserId, FullName, Oid),
    log(debug, "Instrumented walk result: ~p", [Result]),
    snmpm:unregister_agent(UserId, FullName).

walk(UserId, TargetName, Oid) ->
    StartTime = erlang:system_time(milli_seconds),
    walk_r(StartTime, UserId, TargetName, Oid, Oid, same_oid_retries()).

walk_r(_StartTime, _UserId, _TargetName, _FirstOid, _CurrentOid, 0) ->
    count("failed_walk"),
    all_failed;
walk_r(StartTime, UserId, TargetName, FirstOid, CurrentOid, Attempts) ->
    case get_next(UserId, TargetName, CurrentOid) of
	timeout ->
	    walk_r(StartTime, UserId, TargetName, FirstOid, CurrentOid, Attempts - 1);
	{NextOid, _Value, _IndexThing} ->
	    case lists:prefix(FirstOid, NextOid) of
		true ->
		    Elapsed = erlang:system_time(milli_seconds) - StartTime,
		    histo("full_walk", Elapsed);
		false ->
		    walk_r(StartTime, UserId, TargetName, FirstOid, NextOid, same_oid_retries())
	    end
    end.

get_next(UserId, TargetName, Oid) ->
    count("sync_get_next"),
    {Time, Result} =
	timer:tc(
	  fun() -> snmpm:sync_get_next(UserId, TargetName, [Oid], single_oid_timeout()) end),
    case Result of
	{error, {timeout, _}} ->
	    log(debug, "sync_get_next timeout on ~p", [Oid]),
	    count("timeout"),
	    timeout;
	{ok, {noError, _, [{varbind, NextOid, _Type, Value, IndexThing}]}, _} ->
	    histo("response_time", Time/1000),
	    {NextOid, Value, IndexThing}
    end.

count(MetricName) ->
    gen_server:cast(snmp_profiler_stats, {count, MetricName}).

histo(MetricName, Value) ->
    gen_server:cast(snmp_profiler_stats, {histo, MetricName, Value}).


%% @doc Number of times to retry a single oid during a walk. If this number is exceeded,
%% the walk is considered a failure. Might want to make this configurable.
same_oid_retries() -> 5.

%% @doc Timeout for a single sync_get_next() call. Might want to make this configurable.
single_oid_timeout() -> timer:seconds(5).

%% @doc The OID to walk. Might want to make this configurable.
oid_to_walk() -> [1,3,6,1,2,1,17,1,4,1,2].

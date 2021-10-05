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
    run_switch("aggr301a-3.sjc3"),
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
    {ok, Address} = inet:getaddr(FullName, inet),
    Oid = [1,3,6,1,2,1,17,1,4,1,2],
    log(debug, "Address for ~s is ~p", [FullName, Address]),
    Opts = [{engine_id, "manager's engine"},
	    {address,   Address},
	    {community, snmp_profiler_config:community_string()},
	    {version,   v2},
	    {sec_model, v2c}],
    log(debug, "Register SNMP agent for ~s", [FullName]),
    UserId = "snmp_profiler",
    case snmpm:register_agent(UserId, FullName, Opts) of
	ok -> ok;
	Error ->
	    Msg = io_lib:format("Failed to register SNMP agent:\n~p", [Error]),
	    die(Msg, [internal])
    end,
    Result = instrumented_walk(UserId, FullName, Oid),
    log(debug, "Instrumented walk result: ~p", [Result]),
    snmpm:unregister_agent(UserId, FullName).

instrumented_walk(UserId, TargetName, Oid) ->
    gen_server:cast(snmp_profiler_stats, {count, "sync_get_next"}),
    {Time, Result} = timer:tc(fun() -> snmpm:sync_get_next(UserId, TargetName, [Oid]) end),
    case Result of
	{error, {timeout, _}} ->
	    gen_server:cast(snmp_profiler_stats, {count, "timeout"}),
	    timeout;
	{ok, {noError, _, [{varbind, _NextOid, _Type, Value, IndexThing}]}, _} ->
	    gen_server:cast(snmp_profiler_stats, {histo, "response_time", Time/1000}),
	    {Value, IndexThing}
    end.

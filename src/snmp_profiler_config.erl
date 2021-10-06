-module(snmp_profiler_config).
-export([
	 parse_input/1,
	 stop/0,
	 dump/0,
	 verbose/0,
	 community_string/0,
	 test_one/0
	]).
-import(snmp_profiler_utils, [die/2, log/3]).

parse_input(Args) ->
    ets:new(snmp_profiler_config, [public, named_table]),
    parse_args(Args).

stop() ->
    ets:delete(snmp_profiler_config).

ins(K, V) ->
    ets:insert(snmp_profiler_config, {K, V}).

getk(K) ->    
    case ets:lookup(snmp_profiler_config, K) of
	[{K, V}] -> V;
	[] -> 
	    die(io_lib:format("Unknown config key '~s'", [K]), [internal])
    end.

%% @doc Will parse cmdline args and return a good config or die trying.
parse_args([]) ->
    validate_args(target);
parse_args(["dc", DC | T]) ->
    ins("dc", DC),
    parse_args(T);
parse_args(["test-one", SwitchName | T]) ->
    ins("test-one", SwitchName),
    parse_args(T);
parse_args(["verbose", Level | T]) ->
    ins("verbose", list_to_integer(Level)),
    parse_args(T);
parse_args(["community-string", String | T]) ->
    ins("community-string", String),
    parse_args(T);
parse_args([H|_]) ->
    die(io_lib:format("Unrecognized argument: ~s", [H]), [usage]).

validate_args(target) ->
    case {getk("dc"), getk("test-one")} of
	{"no_dc_given", "no_test_one_given"} ->
	    die("Missing required option: one of --dc or --test-one must be given", [usage]);
	_ ->
	    validate_args("community-string")
    end;
validate_args("community-string") ->
    case getk("community-string") of
	"no_community_string_given" ->
	    die("Missing required option --community-string", [usage]);
	_ ->
	    ok
    end.

dump() ->
    maps:from_list(ets:tab2list(snmp_profiler_config)).

verbose() ->
    getk("verbose").

community_string() ->
    getk("community-string").

test_one() ->
    getk("test-one").

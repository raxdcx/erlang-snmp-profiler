% Copyright 2021 Rackspace Technology
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
-module(snmp_profiler_config).
-export([
	 parse_input/1,
	 stop/0,
	 unset_config_value/0,
	 dump/0,
	 verbose/0,
	 community_string/0,
	 test_one/0,
	 datafile/0
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
parse_args(["datafile", File | T]) ->
    ins("datafile", File),
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
    case {getk("datafile"), getk("test-one")} of
	{"unset_config_value", "unset_config_value"} ->
	    die("Missing required option: one of --datafile or --test-one must be given", [usage]);
	_ ->
	    validate_args("community-string")
    end;
validate_args("community-string") ->
    case getk("community-string") of
	"unset_config_value" ->
	    die("Missing required option --community-string", [usage]);
	_ ->
	    ok
    end.

%% @doc If a config item is set to this value, it means the user
%% didn't specify a value for it.
unset_config_value() ->
    "unset_config_value".

dump() ->
    Raw = maps:from_list(ets:tab2list(snmp_profiler_config)),
    Raw#{"community-string" := "********"}.

verbose() ->
    getk("verbose").

community_string() ->
    getk("community-string").

test_one() ->
    getk("test-one").

datafile() ->
    getk("datafile").

#!/bin/bash -e
# Copyright 2021 Rackspace Technology
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# You can't pass hyphenated args to an erlang process, so
# to make the arg handling nice, I'm preprocessing them here
# with getopt. Each long argument name here corresponds to
# a config item in the erlang program.
temp=$(getopt -o hvc:f:o: --long help,verbose,community-string:,datafile:,test-one: -n "$0" -- "$@")

[ $? -eq 0 ] || { echo "Terminating..." >&2; exit 1; }

eval set -- "$temp"

verbose=0
datafile=unset_config_value
community_string=unset_config_value
test_one=unset_config_value

while true; do
    case "$1" in
	-h | --help ) erl -pa ebin -noshell -run snmp_profiler_utils usage -s erlang halt; exit 0 ;;
	-v | --verbose ) verbose=$((verbose+1)); shift ;;
	-f | --datafile ) datafile="$2"; shift 2 ;;
	-c | --community-string ) community_string="$2"; shift 2 ;;
	-o | --test-one ) test_one="$2"; shift 2 ;;
	-- ) break ;;
    esac
done

erl -pa ebin -noshell -run snmp_profiler profile \
    verbose $verbose \
    datafile $datafile \
    community-string $community_string \
    test-one $test_one \
    -s init stop

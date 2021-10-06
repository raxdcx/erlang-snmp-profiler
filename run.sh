#!/bin/bash -e

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

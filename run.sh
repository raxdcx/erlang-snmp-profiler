#!/bin/bash -e

# You can't pass hyphenated args to an erlang process, so
# to make the arg handling nice, I'm preprocessing them here
# with getopt. Each long argument name here corresponds to
# a config item in the erlang program.
temp=$(getopt -o vc:d:o: --long verbose,community-string:,dc:,test-one: -n "$0" -- "$@")

[ $? -eq 0 ] || { echo "Terminating..." >&2; exit 1; }

eval set -- "$temp"

verbose=0
dc=no_dc_given
community_string=no_community_string_given
test_one=no_test_one_given

while true; do
    case "$1" in
	-v | --verbose ) verbose=$((verbose+1)); shift ;;
	-d | --dc ) dc="$2"; shift 2 ;;
	-c | --community-string ) community_string="$2"; shift 2 ;;
	-o | --test-one ) test_one="$2"; shift 2 ;;
	-- ) break ;;
    esac
done

erl -pa ebin -noshell -run snmp_profiler profile \
    verbose $verbose \
    dc $dc \
    community-string $community_string \
    test-one $test_one \
    -s init stop

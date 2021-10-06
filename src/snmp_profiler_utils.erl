-module(snmp_profiler_utils).
-export([die/2, log/3]).

die(Message, Opts) ->
    Prefix = case proplists:get_value(internal, Opts, false) of
		 true -> "INTERNAL ERROR";
		 false -> "FATAL"
	     end,
    io:format("~s - ~s\n", [Prefix, Message]),
    case proplists:get_value(usage, Opts, false) of
	true ->
	    io:format("\n"),
	    usage();
	false ->
	    ok
    end,
    erlang:halt(1).

usage() ->
    Usage = 
	[
	 "Usage: ./run.sh [OPTION]...",
	 "Test internal performance of erlang SNMP packet handling.",
	 "",
	 "  -c, --community-string=STRING",
	 "                   specify the SNMP v2c community string to use",
	 "  -f, --datafile=FILE",
	 "                   specify a file containing a list of switch names to test",
	 "                   see DATAFILES; either this or --test-one must be provided",
	 "  -o, --test-one=SWITCHNAME",
	 "                   test performance of a single switch; either this or the",
	 "                   --datafile option must be provided",
	 "  -v, --verbose    increase output verbosity; may be used multiple times",
	 "",
	 "DATAFILES",
	 "",
	 "  TBD"
	],
    io:format([string:join(Usage, "\n"), "\n"]).

%% @doc Logs stuff at different levels. Suport usual levels of error, info,
%% and debug, activated by passing one or more '-v' options to the runner
%% script. Also supports 'user' as a value, which instead of formatting as
%% a log, just prints the given output to the user. This output is always
%% on, no matter what, as is error logging.
log(user, Format, Args) ->
    io:format(lists:flatten(["USER  - ", Format, "\n"]), Args);
log(Level, Format, Args) ->
    LevelNumber = maps:get(Level, #{
			     error => 0,
			     info => 1,
			     debug => 2
			    }),
    case LevelNumber =< snmp_profiler_config:verbose() of
	false ->
	    ok;
	true ->
	    LevelName = string:to_upper(atom_to_list(Level)),
	    io:format("~-5s - ~s\n", [LevelName, io_lib:format(Format, Args)])
    end.

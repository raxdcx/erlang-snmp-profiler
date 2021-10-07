# Erlang SNMP Profiler

I observed some strang behavior with Erlang's SNMP in which SNMP
 response packets would arrive at a server's network interface fairly
 promptly, but there was a big delay between the packet's arrival and
 the time the Erlang function returned the data. This is a small
 program to give some profiling data about SNMP performance. Run it
 from various locations and pointing at various targets to compare
 performance. We hope this will help narrow down the performance
 problem.

## Dev mode

This is meant to run in an Erlang Docker container for easy
portability. Start one from this project dir with

    docker run --rm -it -v $(pwd):/app -w /app -p 21312:21312 \
        erlang:18.3.4.11 bash

Three shell scripts give you basic functionality:

- `compile.sh` compiles the project

- `run.sh` is the main runner for both dev and real usage; compile first

- `clean.sh` cleans compiled artifacts; could also just `git clean -fdx`

## Distribution

This is TBD, but I'll come up with a way to bundle this up nicely so
that anyone with access to a server can run the `run.sh` script to run
performance tests.

## Design

The goal is to test Erlang [snmpm:sync_get_next()](
https://erlang.org/doc/man/snmpm.html#sync_get_next2-3) calls. In
troubleshooting, I've observed that this function sends a request
packet, and the response packet is received in very short order
according to a tcpdump of the interface; however there's a long delay
before the erlang function returns the data. This project will focus
on observing the behavior of that function.

It allows you to test the performance of a single device or a group of
them defined by an input file. For each test run, it produces a report
with useful data for comparing against other runs. It will be run by
non-developers, so its interface needs to be relatively clean and easy
to use.

The `run.sh` script does initial input arg parsing and passes all args
through to the erlang program.  A dedicated config module further
interprets, validates, and stores all the input arguments so they're
sanitized and available to the rest of the application. For sanity,
the arg names are the same at the shell script level as inside the
erlang program, except that in some places, hyphens have to become
underscores because of language constraints.

A primitive logging mechanism lets users choose between three levels
of verbosity via command line flags. All logs are written to stdout.

The program should abort with a good error message and non-zero exit
status for any recognizable error.  There's a `die()` function for
this purpose.  Be sure to tag internal failures as such. An example of
an internal failure would be asking the config module for a config
item that doesn't exist.

#!/bin/bash -e

mkdir -p ebin
erlc -o ebin src/*.erl

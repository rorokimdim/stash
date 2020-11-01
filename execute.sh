#!/usr/bin/env bash

cmd=$1
shift

stack exec stash $cmd -- "$@"

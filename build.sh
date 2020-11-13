#!/usr/bin/env bash

set -e

PRETTY_COMMAND="brittany"
PRETTY_COMMAND_ARGS="--write-mode inplace"

check_stack() {
    if ! hash stack 2>/dev/null; then
        echo "Please install stack first. See https://docs.haskellstack.org/en/stable/README/"
        exit 1
    fi
}

check_hlint() {
    if ! hash hlint 2>/dev/null; then
        echo "Please install hlint first. Run the command, stack install hlint"
        exit 1
    fi
}

prettify() {
    if hash $PRETTY_COMMAND 2>/dev/null; then
        echo "Prettifying code with $PRETTY_COMMAND..."
        find src app test benchmark -name "*.hs" | xargs -n1 $PRETTY_COMMAND $PRETTY_COMMAND_ARGS
    else
        echo "Warning: $PRETTY_COMMAND command not found. Try: stack install $PRETTY_COMMAND"
    fi
}

get_next_version() {
    echo `cat package.yaml | awk -F ":" '/version/ {print $2}' | sed 's/ //g'`
}

maybe_update_version() {
    sed -i.backup "s/appVersion =.*/appVersion = \"$(get_next_version)\"/" stash.org
    rm -rf stash.org.backup
    sed -i.backup "s/appVersion =.*/appVersion = \"$(get_next_version)\"/" app/Main.hs
    rm -rf app/Main.hs.backup
}

check_stack
check_hlint

prettify
maybe_update_version
hlint -i "Eta reduce" -X QuasiQuotes src app test benchmark --color

stack build --test
stack install --local-bin-path bin/

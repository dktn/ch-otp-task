#!/bin/bash

run_node () {
    echo "$prefix Run node $current_node at $1:$2"
    ch-opt-task slave $1 $2 &
    current_node=$(expr $current_node + 1)
}

run_nodes () {
    echo "$prefix Starting slave nodes"
    source nodes.sh
    sleep 1
    echo "$prefix Slave nodes started"
}

run_master () {
    echo "$prefix Starting master"
    ch-opt-task master 127.0.0.1 8081 --send-for=5 --wait-for=2 --with-seed=12345
}

prefix="Runner:"
current_node=1
if [ "$1" == "build" ]; then
    stack install
fi
run_nodes
run_master
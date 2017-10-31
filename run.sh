#!/bin/bash

send_for=12
wait_for=10
# send_for=10
# wait_for=15
wait_killall_for=10

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
    ch-opt-task master 127.0.0.1 8081 --send-for=$send_for --wait-for=$wait_for --with-seed=12345 --msg-delay=0 --buffer-size=5000 --time-to-show=700000 &
}

prefix="Runner:"
current_node=1
if [ "$1" == "build" ]; then
    stack install
fi
run_nodes
run_master
sleep $(expr $send_for + $wait_for + $wait_killall_for)
echo "$prefix Killing all processes"
killall -9 ch-opt-task

#!/bin/bash

send_for=60
wait_for=60
wait_killall_for=20
# send_for=10
# wait_for=15
# wait_killall_for=10
seed=12345

prefix="Runner:"
current_node=1

run_node () {
    echo "$prefix Run node $current_node at $1:$2"
    ch-otp-task slave $1 $2 &
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
    ch-otp-task master 127.0.0.1 8081 --send-for=$send_for --wait-for=$wait_for --with-seed=$seed --msg-delay=00000 --buffer-size=500000 --time-to-show=700000 &
    sleep $(expr $send_for + $wait_for + $wait_killall_for)
    echo "$prefix Killing all processes"
    killall -9 ch-otp-task
}

run_program () {
    run_nodes
    run_master
}

if [ "$1" == "build" ]; then
    stack install && run_program
else
    run_program
fi

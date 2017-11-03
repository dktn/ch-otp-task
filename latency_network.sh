#!/bin/bash

# Sets up the network in poor condition, based on the blog:
# https://mop.koeln/blog/2015/06/01/Limiting-bandwidth-on-Mac-OS-X-yosemite/

disable_latency_network () {
    echo "Disabling latency network"
    echo | sudo pfctl -a mop -f -
    cat /etc/pf.conf | sudo pfctl -f -
    sudo dnctl -f flush
}

enable_latency_network () {
    echo "Enabling latency network"
    sudo pfctl -e
    (cat /etc/pf.conf && echo "dummynet-anchor \"mop\"" && echo "anchor \"mop\"") | sudo pfctl -f -
    sudo dnctl pipe 1 config delay 5
    sudo dnctl pipe 2 config delay 50
    sudo dnctl pipe 3 config delay 100
    sudo dnctl pipe 4 config delay 200
    sudo dnctl pipe 5 config delay 500
    sudo dnctl pipe 6 config delay 100
    sudo dnctl pipe 7 config delay 50
    (
        echo "dummynet in  quick proto tcp from any to any port 8081 pipe 1"
        echo "dummynet out quick proto tcp from any to any port 8081 pipe 1"
        echo "dummynet in  quick proto tcp from any to any port 8082 pipe 2"
        echo "dummynet out quick proto tcp from any to any port 8082 pipe 2"
        echo "dummynet in  quick proto tcp from any to any port 8083 pipe 3"
        echo "dummynet out quick proto tcp from any to any port 8083 pipe 3"
        echo "dummynet in  quick proto tcp from any to any port 8084 pipe 4"
        echo "dummynet out quick proto tcp from any to any port 8084 pipe 4"
        echo "dummynet in  quick proto tcp from any to any port 8085 pipe 5"
        echo "dummynet out quick proto tcp from any to any port 8085 pipe 5"
        echo "dummynet in  quick proto tcp from any to any port 8086 pipe 6"
        echo "dummynet out quick proto tcp from any to any port 8086 pipe 6"
        echo "dummynet in  quick proto tcp from any to any port 8087 pipe 7"
        echo "dummynet out quick proto tcp from any to any port 8087 pipe 7"
    ) | sudo pfctl -a mop -f -
}

if [ "$1" == "disable" ]; then
    disable_latency_network
else
    enable_latency_network
fi

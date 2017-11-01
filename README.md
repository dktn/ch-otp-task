# ch-otp-task

CH/OTP Test Task Solution by Adam Szlachta


Quick setup:

1. Run `stack setup`
2. Edit `nodes.sh` file to setup worker nodes (with host address and port number for each).
3. Run `./run.sh build` in order to build, install (to `~/.local/bin`) and run the system.
4. Subsequent runs can be shortened to `./run.sh` (no build).


Configuration:
- Basic options
- Extra options

Running:
- steps

Implementation:
- Priority queue
- Terminating all slaves takes significant amount of time

Trade-offs:
- Can't be solved in general
- Buffer limited by memory, limiting by timeouts problematic (can grow if there are many nodes)

Ideas:
- Broadcasting
- Ask to resend



Questions:
1. Do several nodes send messages to other nodes including these several nodes? And including themselves? Are any nodes in any way special? Should I make possible to configure which nodes are senders (and presumably receivers), and which are only receivers?

2. Should I make sure that even given one seed for each node in the cluster, each node yields different sequence of numbers? It's achievable by mangling the seed with node id or port number. If not there is a possibility to calculate the result quite deterministically, just by knowing how many other nodes are in the cluster.

3. Is getting the biggest score a priority, or maybe fault tolerance is the most important?

4. Will the fault tolerance be tested during execution? I'd assume that yes, but "failure maps" suggest something statically configured.

5. Do all nodes have to yield the same result? Can I assume that some messages can be lost, and the results will differ? Even if not, there is a possibility that a node will want to skip a received message, because it already took another message with newer timestamp into sum calculation. Generally I assume that the sum will be calculated online, without keeping the history, to avoid space leaks on longer execution times.

6. There are sending and waiting stages. The end of waiting stage is when the nodes are killed. How to announce the end of sending stage? Can master program send a special message to all nodes, or the nodes themselves should announce the end mutually? Or maybe it does not really matter?

7. Since master node knows all slaves I'm currently initiating slave senders with the list of nodes so they can know who to send to and how many nodes exist. Is it acceptable? Or should I additionally implement some kind of node discovery?



Sample runs:

Machine: MacBook Pro mid-2012, 2,7 GHz Intel Core i7 (4 cores), 16 GB 1600 MHz DDR3

10 nodes, 1 minute, no throttling
```
Wed Nov  1 01:01:42 UTC 2017 pid://127.0.0.1:8081:0:8: Config:
        {_sendDuration = 60, _waitDuration = 30, _seed = 12345, _msgDelay = 0, _msgBuffer = 500000, _timeToShow = 700000}
Wed Nov  1 01:01:42 UTC 2017 pid://127.0.0.1:8081:0:8: Sending for 60 second(s)
Wed Nov  1 01:02:42 UTC 2017 pid://127.0.0.1:8081:0:8: Waiting for 30 second(s)
Wed Nov  1 01:02:42 UTC 2017 pid://127.0.0.1:8084:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 1544
Wed Nov  1 01:02:43 UTC 2017 pid://127.0.0.1:8087:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 3479
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8083:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 6320
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8088:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 6558
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8089:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 4559
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8090:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 3824
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8085:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 6199
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8086:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 3345
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8091:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 5800
Wed Nov  1 01:02:44 UTC 2017 pid://127.0.0.1:8082:0:10: Final result: <281243, 1.9754308654369343e10> all-finished max-buffer: 5289
Wed Nov  1 01:03:14 UTC 2017 pid://127.0.0.1:8081:0:8: Slaves terminated
```

24 nodes, 1 minute, no throttling
```
Wed Nov  1 00:53:57 UTC 2017 pid://127.0.0.1:8081:0:8: Config: MasterConfig
        {_sendDuration = 60, _waitDuration = 30, _seed = 12345, _msgDelay = 0, _msgBuffer = 500000, _timeToShow = 700000}
Wed Nov  1 00:53:57 UTC 2017 pid://127.0.0.1:8081:0:8: Sending for 60 second(s)
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8081:0:8: Waiting for 30 second(s)
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8088:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6298
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8099:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 3247
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8089:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 3738
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8105:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6129
Wed Nov  1 00:54:57 UTC 2017 pid://127.0.0.1:8083:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6606
Wed Nov  1 00:54:58 UTC 2017 pid://127.0.0.1:8090:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 5396
Wed Nov  1 00:54:58 UTC 2017 pid://127.0.0.1:8085:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 5638
Wed Nov  1 00:54:59 UTC 2017 pid://127.0.0.1:8094:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 4402
Wed Nov  1 00:54:59 UTC 2017 pid://127.0.0.1:8101:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6303
Wed Nov  1 00:54:59 UTC 2017 pid://127.0.0.1:8095:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 10893
Wed Nov  1 00:54:59 UTC 2017 pid://127.0.0.1:8093:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 10964
Wed Nov  1 00:54:59 UTC 2017 pid://127.0.0.1:8087:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 8760
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8102:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6947
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8086:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 7767
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8098:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 7124
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8104:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 5128
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8091:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 12230
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8100:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 10766
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8103:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 6856
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8084:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 15342
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8092:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 13915
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8097:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 13585
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8082:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 14715
Wed Nov  1 00:55:00 UTC 2017 pid://127.0.0.1:8096:0:10: Final result: <96516, 2.3380171779276013e9> all-finished max-buffer: 12824
Wed Nov  1 00:55:29 UTC 2017 pid://127.0.0.1:8081:0:8: Slaves terminated
```


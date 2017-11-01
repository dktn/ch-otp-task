# CH/OTP Test Task

Task solution by Adam Szlachta


## 1 Quick setup

1. Run `stack setup`
2. Edit `nodes.sh` file to setup worker nodes (with host address and port number for each).
3. Run `./run.sh build` in order to build, install (to `~/.local/bin`) and run the system.
4. Subsequent runs can be shortened to `./run.sh` (no build).


## 2 Configuration

### 2.1 Basic options

#### `master` and `slave` commands

Node can be run as master or slave by using commands `ch-otp-task master` and `ch-otp-task slave`.
To get help simply add `--help`.

The result of `ch-otp-task --help`:
```
ch-otp-task - CH/OTP Test Task

Usage: ch-otp-task COMMAND
  Runs distributed task

Available options:
  -h,--help                Show this help text

Available commands:
  slave                    Runs slave process
  master                   Runs master process
```

##### Slave
The result of `ch-otp-task slave --help`:
```
Usage: ch-otp-task slave HOST PORT
  Runs slave process

Available options:
  -h,--help                Show this help text
  HOST                     Host name
  PORT                     Port number
```
The only parameters to pass to slave is `host` (ip address) and `port` on which it runs.
Slave does not do anything until it's set up by master process.
Example usage: `ch-otp-task slave 127.0.0.1 8082 &`.
Note: the task is run in the background in order to be able to run many slave nodes easily from the same terminal session.

##### Master
The result of `ch-otp-task master --help`:
```
Usage: ch-otp-task master HOST PORT (-s|--send-for SECONDS)
                          (-w|--wait-for SECONDS) [-r|--with-seed SEED]
                          [-d|--msg-delay MICROSECONDS]
                          [-b|--buffer-size NUMBER]
                          [-t|--time-to-show MICROSECONDS]
  Runs master process

Available options:
  -h,--help                Show this help text
  HOST                     Host name
  PORT                     Port number
  -s,--send-for SECONDS    Time to send messages
  -w,--wait-for SECONDS    Time to wait for the result
  -r,--with-seed SEED      Seed for random numbers generator (default: 0)
  -d,--msg-delay MICROSECONDS
                           Delay between sending messages (default: 100)
  -b,--max-buffer-size NUMBER
                           Size of messages buffer (default: 500000)
  -t,--time-to-show MICROSECONDS
                           Time needed to calculate and show
                           result (default: 700000)
```
The obligatory parameters for `master` are `host`, `port` number and `--send-for` with `--wait-for` values. The remaining values have defaults displayed by help message.

The parameters `--send-for`, `--wait-for` and `--with-seed` are defined in the task description.

### 2.2 Additional options

The remaining parameters for the `master` have following meaning:
- `--msg-delay` - delay in microseconds before sender sends the message. It allows for throttling the speed of message generators. Defaults to 0. __Warning:__ for some reasons, in some circumstances the small but non-zero values can give unexpected behaviour and hang the system (on MacOS Sierra). Value `0` seems to be fine in most of the cases, but if not a values at least `10000`-`100000` microseconds is recommended.
- `--max-buffer-size` - the maximum size of the buffer in the receiver, in the number of messages. When the value of this buffer is exceeded the incoming messages will be ignored.
- `--time-to-show` - this is the time in microseconds before the end of wait period. It's an absolute deadline to show the results of calculations, but in many cases the result will be calculated much quicker.


## 3 Running

### 3.1 Preparation

To properly run the system the `ch-otp-task` must be compiled and copied to somewhere on `PATH` list.
Typically it may be done using following steps:
1. Run `stack setup` - this command downloads, compiles and installs the Haskell compiler __GHC 8.2.1__ managed by __stack__ tool. The GHC version is connected to the `resolver` section in `stack.yaml` settings, which currently is set to `resolver: nightly-2017-10-26`.
2. Run `stack install` - this command compiles the `ch-otp-task` program and copies the binary to local binaries directory, typically to `~/.local/bin`. It's an alias to `stack build --copy-bins`.

### 3.2 Manual start

Entire system can be run manually in two steps:
1. Run one or more _slave_ nodes.
2. Run the _master_ node.

The nodes should be set up on available hosts and unoccupied ports. Currently the incorrect setups are not handled (TODO).

#### Example of the manual run

On one slave node:

```
~/dev/ch-otp-task $ ch-otp-task slave 127.0.0.1 8082 &
[1] 57701
~/dev/ch-otp-task $ ch-otp-task master localhost 8081 --send-for=3 --wait-for=1
Wed Nov  1 21:27:55 UTC 2017 pid://localhost:8081:0:8: Config: MasterConfig {_sendDuration = 3, _waitDuration = 1, _seed = 0, _msgDelay = 0, _msgBuffer = 500000, _timeToShow = 700000}
Wed Nov  1 21:27:55 UTC 2017 pid://localhost:8081:0:8: Sending for 3 second(s)
Wed Nov  1 21:27:58 UTC 2017 pid://127.0.0.1:8082:0:10: Final result: <206619, 1.0672638883080137e10> all-finished max-buffer: 2
Wed Nov  1 21:27:58 UTC 2017 pid://localhost:8081:0:8: Waiting for 1 second(s)
[1]  + 57701 done       ch-otp-task slave 127.0.0.1 8082
Wed Nov  1 21:28:01 UTC 2017 pid://localhost:8081:0:8: Slaves terminated
```

On two slave nodes:
```
~/dev/ch-otp-task $ ch-otp-task slave 127.0.0.1 8082 &
[1] 60198
~/dev/ch-otp-task $ ch-otp-task slave 127.0.0.1 8083 &
[2] 60229
~/dev/ch-otp-task $ ch-otp-task master localhost 8081 --send-for=3 --wait-for=1
Wed Nov  1 23:26:45 UTC 2017 pid://localhost:8081:0:8: Config: MasterConfig {_sendDuration = 3, _waitDuration = 1, _seed = 0, _msgDelay = 0, _msgBuffer = 50000, _timeToShow = 700000}
Wed Nov  1 23:26:45 UTC 2017 pid://localhost:8081:0:8: Sending for 3 second(s)
Wed Nov  1 23:26:48 UTC 2017 pid://127.0.0.1:8083:0:10: Final result: <94492, 2.2371742255130634e9> all-finished max-buffer: 161
Wed Nov  1 23:26:48 UTC 2017 pid://127.0.0.1:8082:0:10: Final result: <94492, 2.2371742255130634e9> all-finished max-buffer: 115
Wed Nov  1 23:26:48 UTC 2017 pid://localhost:8081:0:8: Waiting for 1 second(s)
[2]  + 60229 done       ch-otp-task slave 127.0.0.1 8083
[1]  + 60198 done       ch-otp-task slave 127.0.0.1 8082
Wed Nov  1 23:26:51 UTC 2017 pid://localhost:8081:0:8: Slaves terminated
```

### 3.3 Automatic start

1. Edit `nodes.sh` file to setup slave nodes (with host address and port number for each). It contains the calls to `bash` function running slave nodes. The format of the file is straightforward - an example entry is `run_node 127.0.0.1 8082`.
3. Run `./run.sh build` in order to build the program, install it (to `~/.local/bin`) and run the system.
4. If the the program (`ch-otp-task`) is already compiled and copied, it is enough to use `./run.sh`.

#### Examples of automatic runs
Examples of computation intensive automatic runs for 10 and 24 nodes are shown below.

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

## 4 Implementation

### 4.1 Short description

Master node runs and synchronizes the start of worker processes on slave nodes. The worker nodes have two processes each, one for sending, and the other one for receiving the messages and calculating the results. Together with the random value a timestamp is passed in the message. A receiver will store the messages in priority queue which acts as a buffer in order to achieve the correct ordering by timestamps. The messages with minimal timestamps (the oldests) are taken out of the queue in order to calculate the required sum online. This avoids the space leaks. When all the senders are done they inform receivers about it and the receivers calculate the remaining part of the sum from all the remaining messages from the queue. After that they display the results, but if there is an absolute timeout (risk of being killed) they display the result earlier.

### 4.2 Long description


- Priority queue
- Terminating all slaves takes significant amount of time

### 4.3 Trade-offs:
- Can't be solved in general
- Buffer limited by memory, limiting by timeouts problematic (can grow if there are many nodes)

### 4.4 TODO list

1. Add protection against incorrect setups, for example with nodes started on "busy" ports.
2. Check the suspicious behaviour (hanging system) in some rare cases for small delay times - possibly a bug in High Sierra or distributed-process itself.
3. Terminating all slaves takes significant amount of time - investigate.
4. Add CPU/memory benchmarks for various data structures and algorithms.

### 4.5 Ideas:
- Broadcasting
- Peer discovery
- Resends
- Refactor to be able to easily abstract over data structures used
- Use different data structures



## 5 Questions

1. Do several nodes send messages to other nodes including these several nodes? And including themselves? Are any nodes in any way special? Should I make possible to configure which nodes are senders (and presumably receivers), and which are only receivers?

2. Should I make sure that even given one seed for each node in the cluster, each node yields different sequence of numbers? It's achievable by mangling the seed with node id or port number. If not there is a possibility to calculate the result quite deterministically, just by knowing how many other nodes are in the cluster.

3. Is getting the biggest score a priority, or maybe fault tolerance is the most important?

4. Will the fault tolerance be tested during execution? I'd assume that yes, but "failure maps" suggest something statically configured.

5. Do all nodes have to yield the same result? Can I assume that some messages can be lost, and the results will differ? Even if not, there is a possibility that a node will want to skip a received message, because it already took another message with newer timestamp into sum calculation. Generally I assume that the sum will be calculated online, without keeping the history, to avoid space leaks on longer execution times.

6. There are sending and waiting stages. The end of waiting stage is when the nodes are killed. How to announce the end of sending stage? Can master program send a special message to all nodes, or the nodes themselves should announce the end mutually? Or maybe it does not really matter?

7. Since master node knows all slaves I'm currently initiating slave senders with the list of nodes so they can know who to send to and how many nodes exist. Is it acceptable? Or should I additionally implement some kind of node discovery?



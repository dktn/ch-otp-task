# ch-opt-task

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

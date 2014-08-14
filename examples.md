---
title: Delta Examples
layout: default
---


Swap <a name="Swap"> </a>
----

The [swap](examples/swap.delta) example demonstrates parallel
composition. It is more general than parallel assignment.


Factorize <a name="Factorize"> </a>
---------

The [factorize](examples/factorize.delta) example shows how to use the
basic functionalily of Delta.


Greatest Common Divisor <a name="Gcd"> </a>
-----------------------

The [gcd](examples/gcd.delta) example shows first class states.


Quicksort and Heapsort <a name="Sort"> </a>
----------------------

The [quicksort](examples/quicksort.delta) and the
[heapsort](examples/heapsort.delta) examples demonstrate modules. The
main module [sort](examples/sort.delta) uses the two sort libraries.

The quicksort example also demonstrates parallel composition. Change
the `|` into `;` to see how easy it is to
switch to and from sequential composition.


Dining Philosophers <a name="DiningPhilosophers"> </a>
-------------------

The [dining philosophers](examples/dining-philosophers.delta) example
is Dijkstra's classic problem and uses the original solution with
semaphores. It demonstrates how threads are run with parallel
composition. Again, change the `|` into `;` to see how easy it is to
switch to and from sequential composition.


Message Streams <a name="Messages"> </a>
---------------

Delta features communication channels.

The [message loop](examples/message-loop.delta) shows tail recursion
in recursive parallel calls. This examples runs with flat memory use.

The [message loop bound](examples/message-loop-bound.delta) is a
bounded variant.

These examples show the resemblance to sequential communicating
processes.


Client Server <a name="Sockets"> </a>
-------------

This example demonstrates socket programming. It uses tcp messaging in
addition to Delta's internal communication channels.

The [server](examples/server.delta) uses paralles composition to run
threads for each accepted connection. The exception handling makes the
server robust.

The [client](examples/client.delta) connects to the server.


Ant Colony <a name="AntColony"> </a>
----------

Variation on [Rick Hickey's Ant Colony][Hickey]. Ants collect food in
a square world and they communicate with pheromones. File
[ants](examples/ants.delta) contains the ant simulation and file [ants
world](examples/ants_world.delta) containts the supporting data
structures.

The challenge is to use synchronized threads that scale on multicores:
* Each ant runs in a thread 
* All world mutations are atomic and consistent 
* Each snapshot shows a consistent world 	

Rick Hickey's nice solution uses software transactional memory. Delta
has no software transactional memory so the solution uses locks.

Each cell in the world has a mutex to make mutations atomic. Rendering
is done in a seperate thread that uses one mutex to synchronize with
the GUI. When a mutation in the world occurs a message is send to the
seperate rendering thread. For the renderer all actions are sequential
in time.

[Hickey]: https://www.youtube.com/watch?v=dGVqrGmwOAw

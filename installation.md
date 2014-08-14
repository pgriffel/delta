---
title: Delta Installation
layout: default
---

The compiler is distributed in source form and you need to create an
executable yourself. The sources contain the translated compiler
sources to enable this bootstrap proces. Compiling these scheme files
with DrRacket is an easy two step procedure.


Requirements
------------

The language requires
DrRacket. Visit <a href="http://www.racket-lang.org">racket-lang</a>
to obtain it. This version was created with Racket v.5.2.1.
 

Installing Delta
---------------

To create and install the Delta binary:
1. Unpack the sources to some directory
2. cd to directory delta and do `raco link -n delta runtime/`
3. do `raco exe delta.rkt`
4. Place the binary somewhere the OS will find it


Recompiling Delta
-----------------

To recompile the Delta compiler:
1. cd to directory delta and do 'delta exe delta.delta'
2. Place the binary somewhere the OS will find it
    
You can recompile the standard library as follows:
1. cd to directory delta and do 'delta compile standard.delta'
2. mv standard.rkt runtime/

If you extend the runtime and/or the standard library you have to
update the config.delta file in the compiler accordingly and recompile
it as described in the previous steps.


Troubleshooting
---------------

Installation issues most likely involve provide and requires in
Scheme. Reading the generated sources is probably the best way to
handle any issues.

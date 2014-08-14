Delta
=====

The Delta programming language

Paul Griffioen 2014


Introduction
-----------

Delta is a 'pure' imperative programming language. Its unconventional
semantics allows novel construct like parallel composition and first
class states.

The compiler translates Delta programs to (Racket) Scheme and is
completely written in Delta itself.


Installation
------------

The language requires DrRacket. Visit www.racket-lang.org to obtain it. This 
version was created with Racket v.5.2.1.

To create and install the Delta binary:

1. Unpack the sources to some directory 
2. cd to directory delta and do 'raco link -n delta runtime/'
3. do 'raco exe delta.rkt'
4. Place the binary somewhere the OS will find it


Recompiling Delta
-----------------

You can recompile the compiler as follows:

1. cd to directory delta and do 'delta exe delta.delta'
2. Place the binary somewhere the OS will find it

You can recompile the standard library as follows:

1. cd to directory delta and do 'delta compile standard.delta'
2. mv standard.rkt runtime/

If you extend the runtime and/or the standard library you have to
update the config.delta file in the compiler accordingly and recompile
it as described in the previous steps.


Documentation
-------------

Delta is hosted on [GitHub][home].

[home]: http://pgriffel.github.io/delta/

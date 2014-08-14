---
title: Delta
layout: default
---


Introduction
------------

Delta is a 'pure' imperative programming language. Its unconventional
semantics allows novel construct like parallel composition and first
class states.

The compiler translates Delta programs to Scheme and is completely
written in Delta itself.


Hello World
-----------

Install Delta, put the following in a file, say `foo.delta`, and call
`delta run foo.delta`:

    module foo

    public procedure main(args) = print_line("Hello World!")


Documentation
-------------

* The [installation](installation.html) page describes requirements,
  installing Delta, installing DrRacket, and creating your executable
  compiler.
* The [manual](manual.html) describes how to use the Delta language.
* The [specification](specification.html) lists the available
  functions of the Delta language.
* Some [examples](examples.html) show the main features of Delta.


Download
---------

Download the sources from the GitHub project. They contain the
translated racket files of the compiler allowing you to create your
own executable. You need [DrRacket][drracket] to bootstrap the
compiler and to run Delta programs.

[drracket]: http://www.racket-lang.org


Status
------

The compiler is far from complete. Some shortcomings are:

- The type system checks are completely missing and there is no
  runtime type checking. This results in hard to understand messages
  when an error occurs.
- There are no facilities for data abstraction.
- Threads currently run only on a single core.



License
--------

This software is distributed under the [GNU Lesser General Public
License (LGPL)][lgpl] license. We follow [Racket's
interpretation][license] of the license.

[lgpl]: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
[license]: http://download.racket-lang.org/license.html

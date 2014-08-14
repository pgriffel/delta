---
title: Delta Manual
layout: default
---
    
Contents
--------

1. [Running Programs](#Running)
2. [Delta Programs](#Programs)
   - [Definitions](#Definitions)
   - [Functions](#Functions)
   - [Procedures](#Procedures)
   - [Type Declarations](#Declarations)
3. [Expressions](#Expressions)
   - [Variables](#Variables)
   - [Function Application and Tuples](#FunctionApplication)
   - [Operators](#Operators)	    
   - [Statements](#Statements)
   - [Do](#Do)
   - [Try Catch](#TryCatch)	    
4. [Data Types](#DataTypes)
   - [Numbers](#Numbers)
   - [Strings and Characters](#StringsAndCharacters)
   - [Lists](#Lists)
   - [Arrays](#Arrays)
   - [States](#States)
    
    
Running Programs <a name="Running"> </a> 
----------------

To run the program in file `foo.delta` call `delta
run foo.delta` on the command line. This
creates `.rkt` files for the module and all its
dependencies and runs it with `racket`.

Other commands are `delta exe foo.delta`
or `delta compile foo.delta`. Call `delta
help` with the installed compiler executable for further
information.


Delta Programs <a name="Programs"> </a>
--------------

A Delta program is a collection of modules. The program must have a
procedure `main`. This function is called on startup with the command
line options as argument.

A module starts with the declaration of the module name followed by
includes. The body of a module is a list of definitions. For example

    module foo
    
    include io, my_lib
    
    ...

defines module `foo` that uses Delta module `io` and also uses library
`my_lib` that is in the same directory as module `foo`.



Definitions <a name="Definitions"> </a>
-----------

A definition defines a global name for a constant, a function
or a procedure.

Keyword `define` starts a definition. Some examples:

    define pi = 3.14
    
    define ten = 1 + 2 + 3 + 4
    
    public define inner_planets = ["mercury", "venus", "earth", "mars"]

The expression is evaluated and then known globally in the module
by the given name. Only definitions preceded by
keyword `public` are accesable in other modules.



Functions <a name="Functions"> </a>
---------

A function is a special kind of definition.

    function f(x) = y

is a shorthand for

    define f = fun (x) y end

Here `fun (x) y end` is the syntax for an anonymous function of
argument `x` and body `y`.


Procedures <a name="Procedures"> </a>
----------

A procedure is a function that returns the empty state. It exists only
for its side-effects. See the [`do`](#Do) construct for details.

procedure f(x) = y

is a shorthand for

function f(x) = do y end

Similaryly `proc (x) y end` is syntactic suger
for `fun (x) do y end end`.




Type Declarations <a name="Declarations"> </a>
-----------------

Some experimental syntax for type declarations is supported
but the compiler does no type checking yet. Some examples:

    declare some_numbers :: List(Number)
    declare numbers_sum :: (List(Number)) -> Number

The declarations are ignored and are curently useful for
documentation only.



Expressions <a name="Expressions"> </a>
-----------

The body of a definition is an expression, even for procedures. A
statement is just an expression that yields a state.


Variables <a name="Variables"> </a>
---------

A variable is an identifier of alphanumeric characters starting with a
letter. The only non alphanumeric character allowed is the underscore.

To refer to a global the variable must be preceded by `global`. For
example `global print` refers to the print function.


Function Application and Tuples <a name="FunctionApplication"> </a>
-------------------------------

A function application consists of an identifier followed by a tuple
of arguments. For example `f()`, or `f(x)`, or `f(x,y)`.

Tuples are created with special function `tuple`. It just returns the
argument tuple.

Special function `apply` applies a function to a tuple. So `f(x,y,z)`
equals `apply(f,tuple(x,y,z))`.

Special syntax `call` applies a function to a number of arguments. So
`call(f,x,y,z)` is shorthand for `apply(f,tuple(x,y,z))`.


Operators <a name="Operators"> </a>
---------

Delta supports infix operators. Some operators like `;` or `:=` are
special, but most are just syntactic sugar for binary functions. For
example `x+y` is syntactic sugar for `sum(x,y)`.



Statements <a name="Statements"> </a>
----------

A statement is an expression that yields a state. The composition of
statements however make them special.

A statement cannot be a variable or a function call because that would
make the state statically uncertain. In case of side effects you can
use a [`do`](#Do) to turn an expression into the empty statement.

Identifier `skip` and assignment `a := x` are the basic statements.

Compound states are created with operator `;`, operator `&` or
operator `|`. In all cases the result is the result of the first
argument merged with the result of the second argument. In case of
conflicts the second state dominates.

Sequential composition `;` evalutates its first arguments before the
second argument. The result of the fist argument is visible to the
second argument.

Independent composition `&` evalutates its first arguments before the
second argument. The result of the fist argument is not visible to the
second argument.

Parallel composition `|` evalutates its first arguments and its second
argument parallel. The result of the either argument is not visible to
the other argument.


Let <a name="Let"> </a>
---

Let statement `let ... in ... end` evaluates the second expression
with the state extended with the first statement. For example

    let
        i := 0;
        j := 1
    in
        i+j
    end

The statements need not be restricted to assignments. They can also be
procedure calls or any expression, but not function calls because that
would make the state statically uncertain.


Do <a name="Do"> </a>
--

The purpose of the `do ... end` statement is to evaluate expression
purely for their side-effect. The motivation for its inclusion can be
seen by the following problematic program to print `0, 1, 2, . . . n -
1`:

    let i := 0 in
        while i != n do
            print(i);
            i := i + 1
        end
    end

Two problems in it. First it assumes that `print` is a statement,
which excludes the possibility to make print the identity function
that prints its arguments as side-effect. Secondly it will result in a
state with a value for `i` while we are only interested in the
side-effect. For any statement that is only evaluated for its
side-effect it would be preferable to return the empty state. This is
the purpose of the `do` construct. Expression `do x end` yields the
empty state just as the `skip` statement does except that expression
`x` is evaluated for its side-effect. With the `do` construct both
problems can be solved as follows:

    do
      let i := 0 in
          while i != n do
              do print(i) end;
              i := i + 1
          end
      end
    end



Try Catch <a name="TryCatch"> </a>
---------

In `try ... catch(x) ... end` the first expression is evaluated. When
an error occurs the second expression is evaluted with `x` bound to
the error text. All parallel threads in the dynamic scope of the try
are killed.


Data Types <a name="DataTypes"> </a>
----------

Read the source of the runtime for an accurate overview of the
existing functions. The data types are far from complete.


Numbers <a name="Numbers"> </a>
-------

Numbers are inherited from Scheme directly. The current set of
operations is very ad-hoc.


Strings and Characters <a name="StringsAndCharacters"> </a>
----------------------

String literals are surrounded by quotes. Function `format` is
convenient to create strings. For example 

    format("$x x $y $x y $y", x := "bar"; y := "bar")

gives string `"foo x bar foo y bar"`.

Character literals are written with a backslash. For example `\a`
denotes character `a`. Special cases are `\eof` and `\newline`


Lists <a name="Lists"> </a>
-----

Lists are created and manipulated with functions `empty_list`, `cons`,
`first`, `rest`. Some additional functions exist. See also the
standard lib.


Special notation [x,y,...,z] for lists exists. Is shorthand for
`cons(x, cons(y, ... , cons(z, empty_list())))`


Arrays <a name="Arrays"> </a>
------

Use functions `make_array`, `get`, `set`, etc to work with arrays. See
also the standard lib.


States <a name="States"> </a>
------

Identifier `skip` is the empty state. An assignment `a := x` creates a
one element state, the state mapping variable `a` to value `x`.

Compound states are created with sequential composition operator `;`
or with parallel composition operator `|`. See the section on
[statements](#Statements).


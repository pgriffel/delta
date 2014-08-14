---
title: Delta Specification
layout: default
---

    
Contents
--------
    
1. [Primitives](#Primitives)
2. [Standard Modules](#StandardModules)
   - [Module io](#io)
   - [Module channel](#channel)
   - [Module semaphore](#semaphore)
   - [Module socket](#socket)
   - [Module gui](#gui)
    

Primitives <a name="Primitives"> </a>
----------

Functions and procedures have been added on an if needed basis
and documentation is missing. The samples and the compiler sources
probably are the best guidance on how to use the functions. See
the [Racket
reference manual](http://docs.racket-lang.org/reference) for information on used Scheme functions.
    

The primitives:
    
    apply :: forall a, b: (a -> b, a) -> b
    empty_list :: forall a: () -> List(a)
    array_length :: forall a: (Array(a)) -> Number
    get :: forall a: (Array(a), Number) -> a
    set :: forall a: (Array(a), Number, a) -> {}
    write_line :: forall a: (Handle, a) -> {}
    write :: forall a: (Handle, a) -> {}
    array_from_list :: forall a: (List(a)) -> Array(a)
    rest :: forall a: (List(a)) -> List(a)
    list_length :: forall a: (List(a)) -> Number
    first :: forall a: (List(a)) -> a
    make_array :: forall a: (Number) -> Array(a)
    throw :: forall a: (String) -> a
    print :: forall a: (a) -> {}
    print_line :: forall a: (a) -> {}
    cons :: forall a: (a, List(a)) -> List(a)
    equal :: forall a: (a, a) -> Boole
    not_equal :: forall a: (a, a) -> Boole
    tuple :: forall a: a -> a
    now :: forall: () -> Number
    random :: forall: () -> Number
    home_dir :: forall: () -> String
    ask_line :: forall: () -> String
    not :: forall: (Boole) -> Boole
    string :: forall: (List(Char)) -> String
    random_integer :: forall: (Number) -> Number
    sleep :: forall: (Number) -> {}
    less :: forall: (Number, Number) -> Boole
    less_eq :: forall: (Number, Number) -> Boole
    greater_eq :: forall: (Number, Number) -> Boole
    greater :: forall: (Number, Number) -> Boole
    minus :: forall: (Number, Number) -> Number
    div :: forall: (Number, Number) -> Number
    mod :: forall: (Number, Number) -> Number
    multiply :: forall: (Number, Number) -> Number
    sum :: forall: (Number, Number) -> Number
    characters :: forall: (String) -> List(Char)
    read_num :: forall: (String) -> Number
    run_shell_command :: forall: (String) -> {}
    concatenate :: forall: (String, String) -> String
    format :: forall: (String, {}) -> String
    
    
Standard Modules <a name="StandardModules"> </a>
----------------

These modules need to be included.


Module io <a name="io"> </a>
---------

Functions and procedures for reading and writing files. See the file
`io.rkt` in the runtime directory.
    
    alphabetic :: forall: (Char) -> Boole
    numeric :: forall: (Char) -> Boole
    whitespace :: forall: (Char) -> Boole
    eof :: forall: (Handle) -> Boole
    read_char :: forall: (Handle) -> Char
    peek_char :: forall: (Handle) -> Char
    file_lines :: forall: (Handle) -> List(String)
    file_position :: forall: (Handle) -> Tuple(Number, Number, Number)
    close_file :: forall: (Handle) -> {}
    open_output_file :: forall: (String) -> Handle
    open_input_file :: forall: (String) -> Handle
    
    
Module channel <a name="channel"> </a>
--------------

Delta's own message system. See the file `channel.rkt` in the runtime
directory.
      
    channel_name :: forall a: (Channel(a)) -> String
    send :: forall a: (Channel(a), a) -> {}
    receive :: forall a: (List(Channel(a))) -> {message: a, channel: Channel(a)}
    open_channel :: forall a: (String) -> Channel(a)

    
Module semaphore <a name="semaphore"> </a>
----------------

Synchronization. See the file `semaphore.rkt` in the runtime
directory.
    
    make_semaphore :: forall: (Number) -> Semaphore
    up :: forall: (Semaphore) -> {}
    down :: forall: (Semaphore) -> {}


Module socket <a name="socket"> </a>
-------------

TCP connections. See the file `socket.rkt` in the runtime directory.
    
    receive_from :: forall: (Connection) -> String
    send_to :: forall: (Connection, String) -> {}
    accept :: forall: (Socket) -> Connection
    close :: forall: (Socket) -> {}
    listen_on :: forall: (String) -> Socket
    connect_to :: forall: (String, Number) -> Connection
        

Module gui <a name="gui"> </a>
----------

Graphical user interface functions. See the file `gui.rkt` in the
runtime directory. Just enough for the ants demo.
    
    draw_point :: forall: (DC, Number, Number, Number, Color) -> {}
    draw_line :: forall: (DC, Number, Number, Number, Number, Color) -> {}
    draw_rectangle :: forall: (DC, Number, Number, Number, Number, Color) -> {}
    draw_ellipse :: forall: (DC, Number, Number, Number, Number, Color) -> {}
    rgb :: forall: (Number, Number, Number, Number) -> Color
    color :: forall: (String) -> Color
    make_window :: forall: (String, Number, Number, (Window, DC) -> {}) -> Window
    refresh :: forall: (Window) -> {}
    set_background :: forall: (Window, Color) -> {}

## Hasquito

Hasquito is a small compiler for a smaller language. It'd designed to
illustrate some of the basic principles behind compiling functional
languages, particularly lazy ones.

 - Parsing
 - Type Checking
 - Closure Conversion
 - Lambda Lifting
 - Conversion to STG
 - Compilation of STG

Hasquito doesn't have a RTS yet but will Real Soon Now.

## Language

Hasquito compiles a tiny language that is vaguely similar to ML or
Haskell.

    identity : a -> a = (\(x : a) -> x)
    numbers  : Num    = + 1 1
    constant : Num    = 2
    constantTwo : Num = 3
    main : Num -> Num = identity

Notice that all lambdas *must* explicitly annotate their but local
type variables are *not* universally quantified. In particular
something like

    funny : Num = (\(x : a) -> + x 1)

is legal! `a` will simply be constrained to `Num`. This behavior will
go away eventually.

## Contributing

Interested in contributing? Awesome! Let me know what you're
interested in implementing and I'll guide you further. Please email me
at jozefg AT cmu DOT edu.
 

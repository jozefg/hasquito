## Hasquito

Hasquito is a small compiler for a smaller language. It'd designed to
illustrate some of the basic principles behind compiling functional
languages, particularly lazy ones.

 - ✓ Parsing
 - ✓ Type Checking
 - ✓ Closure Conversion
 - ✓ Lambda Lifting
 - Conversion to STG
 - Compilation of STG
 - Expand primitives with booleans

Hasquito doesn't have a RTS yet but will Real Soon Now.

## Language

Hasquito compiles a tiny language that is vaguely similar to ML or
Haskell.

    identity : a -> a = fun x -> x
    numbers  : Num    = + 1 1
    constant : Num    = 2
    constantTwo : Num = 3
    main : Num -> Num = identity

Notice that there is no sugar for top level functions. It simplifies
parsing greatly. Lambdas are implicitly curried though.

## Contributing

Interested in contributing? Awesome! Let me know what you're
interested in implementing and I'll guide you further. Please email me
at jozefg AT cmu DOT edu.

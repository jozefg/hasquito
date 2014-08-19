## Hasquito

Hasquito is a small compiler for a smaller language. It'd designed to
illustrate some of the basic principles behind compiling functional
languages, particularly lazy ones.

The road map for this project looks something like

 - ✓ Parsing
 - ✓ Type Checking
 - ✓ Closure Conversion
 - ✓ Lambda Lifting
 - ✓ Conversion to STG
 - ✓ Compilation of STG to JS
 - ✓ Full Compiler Tests
 - ✓ Add if expressions
 - ✓ Implement Update Frames
 - Implement Trampolining

## Language

Hasquito compiles a tiny language that is vaguely similar to ML or
Haskell.

    identity : a -> a = fun x -> x;
    numbers  : Num = + 1 1;
    constant : Num = 2;
    constantTwo : Num = 3;
    main : Num = identity constant;

Notice that there is no sugar for top level functions. It simplifies
parsing greatly. Lambdas are implicitly curried though.

There are currently 4 primitive operations: `+`, `-`, `*`, and
`/`. The language also supports a primitive if statement. This takes a
number and if it is 0 runs one expression, otherwise the other.

    main : Num = if (- 1 1) 0 1

Will print 0 when run. Other than that the language is just function
application and lambdas.

A Hasquito program when run will evaluate `main : Num` and print the result.

## Using hasquitoc

To compile pieces of Hasquito code first build the compiler with

    cabal build

This should generate an executable called `hasquitoc`. Next all you
have to do is call `hasquitoc` on the file containing some code. The
compiler will create a file called `out.js` with the runtime system
and compiled code. Run this file with node.js or something similar.

Be sure to take a look at `programs` which has several example
test programs as well as a harness for running them.

## Why Javascript

It's fair to ask we we're bothering to compile everything to STG if
we're just compiling that to JavaScript. The simple reason is that
this project is mostly meant for me to play with compiling a lazy
functional language. There's no pressure to make hasquito run fast or
something like that.

With that in mind I wanted a compilation target that, simply put,
didn't make the RTS suck to write. What finally pushed me towards JS
is that the [js-good-parts][js-lib] library means that compiling to JS
is quite pleasant.

Do note that while we're compiling to JS, we're only using a very
small subset of the features that could easily be found in C or even
assembly. It's just that with JS the code is a lot safer and easier to write.

If you find this so deeply offensive that you can't let this stand,
pull requests welcome :)

## Contributing

Interested in contributing? Awesome! Let me know what you're
interested in implementing and we'll chat further.

Please email me at jozefg AT cmu DOT edu.

[js-lib]: https://hackage.haskell.org/package/js-good-parts-0.0.7

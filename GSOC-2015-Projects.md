PureScript developers are keen to mentor interested students for GSOC 2015, under the Haskell.org organization. This page describes some possible project ideas. We have no shortage of projects, so if there is an area you are interested in, please join #purescript on Freenode IRC, and discuss your ideas.

Projects will be PureScript-themed, but should also benefit the larger Haskell community in some way.

## Mentors

- Phil Freeman (@paf31) - willing to mentor any students interested on working on the compiler (any component, but in particular the typechecker) or associated tools.
- John A. De Goes (@jdegoes) - JVM backend, supercompiler, singleton types, first-class labels, Template PureScript

## Project Ideas

### Source Maps

_Interested mentors_: Phil Freeman

Source maps can aid the debugging process in the browser by converting source positions in the generated Javascript code into source positions in the original PureScript code, effectively letting the user step through the PureScript code directly.

This project would add support for source map generation to the compiler.

### Pursuit Enhancements

_Interested mentors_: Phil Freeman

The Pursuit search engine is used to explore PureScript packages and find functions by name. There are plenty of possible enhancements which could make up a GSOC project:

- Add the ability to search by type, a la Hoogle/Hayoo.
- Integrate `psc-pages` to provide Haddock-style documentation.
- Add support for interactive examples using the Try PureScript API.

### Template PureScript

_Interested mentors_: Phil Freeman, John A. De Goes

We would like to support code generation in the style of Template Haskell. The current plan is to implement Template PureScript quasiquoters in a separate source directory and to update the compiler to run those quasiquoters as part of the compilation pipeline, replacing the existing hard-coded rewrite rules.

This project would involve any or all of:

- Add the ability to send the PureScript AST to and from interpreted PureScript at compile time, by reusing the code from the rewrite rules engine.
- Add support for new flags to `psc*` which would load and compile quasiquoters, and run them as part of the build.
- Add support for the quasiquoter flags to the various build plugins.
- Implement some example quasiquoters, such as `Show`/`Eq`/`Ord` deriving.

### Improved Constraint Support

_Interested mentors_: Phil Freeman

Support for type class constraints in the compiler is currently limited to _checking_ mode, which effectively imposes Haskell's _monomorphism restriction_ globally. This project would lift that restriction.

Another possible avenue is to investigate support for _functional dependencies_ or alternative solutions to the problem of ambiguous types when using multi-parameter type classes.

- Context inference and context simplification
- Investigate functional dependencies, or alternatives.

### Supercompiler for Functional Core

_Interested mentors_: John A. De Goes

Idiomatic functional programming involves the manipulation and composition of functions through the use of function combinators. Thanks to the purity of PureScript, function combinators applied to the same parameters always return the same results. Currently, this fact is not taken advantage of, and as a result, there is substantial redundant evaluation of pure expressions in generated Javascript code.

This project would involve evaluating PureScript code at compile-time to maximally apply functions, and generate these specializations for use in runtime Javascript, with the goal of dramatically reducing and in many cases eliminating the runtime overhead of functional abstraction.

 - Partial evaluator for functional core
 - Function specialization
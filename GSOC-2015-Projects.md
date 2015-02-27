PureScript developers are keen to mentor interested students for GSOC 2015, under the Haskell.org organization. This page describes some possible project ideas. We have no shortage of projects, so if there is an area you are interested in, please join #purescript on Freenode IRC, and discuss your ideas.

Projects will be PureScript-themed, but should also benefit the larger Haskell community in some way.

## Mentors

- Phil Freeman (@paf31) - willing to mentor any students interested on working on the compiler (any component, but in particular the typechecker) or associated tools.
- John A. De Goes (@jdegoes) - JVM backend, supercompiler, singleton types, first-class labels, Template PureScript

## Project Ideas

### Support for new backends

_Interested mentors_: Phil Freeman

The current code generator supports two flavors of Javascript - CommonJS modules, and vanilla JS for the browser. There has been a lot of interest in supporting new backends (C/C++, Erlang, JVM, CLR, PHP, Lua, Python). Many of the codegen steps will be identical to the Javascript backend, so we would like to extract an intermediate "imperative core" language, which will act as the data source to these new backends.

This project could take many different forms, but here are some possible goals:

- Define the imperative core AST (trickier than it sounds - we need to support the features of the existing code generator, while enabling new code generators to be written, and also supporting general-purpose optimizations)
- Provide a way to dump the core representation to a file for use with external backends
- Implement the existing optimizations on the core language.
- Port the Javascript backends to use the new core representation.
- Implement a reference backend (probably an interpreter, which we could use in `psci` in environments where `node` is not available, or undesirable for some reason - sandboxing?)
- Implement a specific backend (JVM is probably the target with the most demand right now, but see above for other ideas)

### Source Maps

Source maps can aid the debugging process in the browser by converting source positions in the generated Javascript code into source positions in the original PureScript code, effectively letting the user step through the PureScript code directly.

This project would add support for source map generation to the compiler.

_Interested mentors_: Phil Freeman

### An IDL for typed Haskell/PureScript communication

_Interested mentors_: Phil Freeman

PureScript has libraries for safe, typed integration with external sources of data, but when integrating with a Haskell backend, or JavaScript component written in another typed language, we still have to write two type class instances to serialize and then deserialize the data to/from JSON. It is essential that these instances be compatible.

This project would replace this approach with the use of a simple, typed interface description language (IDL) which could be used by any language with a Haskell-like type system (Haskell, Elm, PureScript, Fay etc.). The tasks would involve

- Define the IDL, or identify a suitable existing IDL
- Write a parser for the IDL
- Write code generators for multiple target languages
- Implement build-tool plugins (Grunt, Gulp)
- Template Haskell support

### Template PureScript

_Interested mentors_: Phil Freeman, John A. De Goes

We would like to support code generation in the style of Template Haskell. The current plan is to implement Template PureScript quasiquoters in a separate source directory and to update the compiler to run those quasiquoters as part of the build.

This project would involve:

- Write an evaluator for PureScript's functional core.
- Add the ability to send the PureScript AST to and from interpreted PureScript at compile time.
- Add support for new flags to `psc*` which would load and compile quasiquoters, and run them as part of the build.
- Add support for the quasiquoter flags to the various build plugins.
- Implement some example quasiquoters, such as `Show`/`Eq`/`Ord` deriving.

### Improved Constraint Support

_Interested mentors_: Phil Freeman

Support for type class constraints in the compiler is currently limited to _checking_ mode, which effectively imposes Haskell's _monomorphism restriction_ globally.

This project would lift that restriction, and also investigate new, useful types of constraints, such as _row constraints_.

Another possible avenue is to investigate support for _functional dependencies_ or alternative solutions to the problem of ambiguous types when using multi-parameter type classes.

- Context inference and context simplification
- Add support for row constraints (either has/lacks or disjoint sum a la Ermine) to the type checker
- Investigate functional dependencies, or alternatives.

### Pursuit Enhancements

_Interested mentors_: Phil Freeman

The Pursuit search engine is used to explore PureScript packages and find functions by name. There are plenty of possible enhancements which could make up a GSOC project:

- Add the ability to search by type, a la Hoogle/Hayoo.
- Integrate `psc-pages` to provide Haddock-style documentation.
- Add support for interactive examples using the Try PureScript API.

### Supercompiler for Functional Core

_Interested mentors_: John A. De Goes

Idiomatic functional programming involves the manipulation and composition of functions through the use of function combinators. Thanks to the purity of PureScript, function combinators applied to the same parameters always return the same results. Currently, this fact is not taken advantage of, and as a result, there is substantial redundant evaluation of pure expressions in generated Javascript code.

This project would involve evaluating PureScript code at compile-time to maximally apply functions, and generate these specializations for use in runtime Javascript, with the goal of dramatically reducing and in many cases eliminating the runtime overhead of functional abstraction.

 - TODO
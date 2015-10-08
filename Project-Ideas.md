This page is a list of possible project ideas for PureScript newcomers who would like to work on a new library.

The PureScript community has no shortage of project ideas, and as part of a small, growing community, your contributions will have immediate value and visibility. The PureScript IRC channel is a good place to find help.

## Haskell Projects

The compiler and many of its tools are written in Haskell, so if you are a beginning Haskeller, those are good places to find interesting, self-contained projects to work on.

### Compiler

- Improving error messages (see the `error messages` tag on GitHub issues)
- Refactoring `Traversals` modules to reduce duplication (`multiplate`, `lens` etc.)
- Documenting PureScript's type system (work with @paf31)

### Tools

- Porting PSCi to PureScript (talk to @paf31)
- An automatic way of sharing types between Haskell and PureScript safely
- A tool for producing FFI bindings from an input file containing definitions (YAML perhaps? `.d.ts` files have been suggested before, but the types don't quite work out)
- Typed holes in `psc-ide`
- A PureScript package manager (needs discussion)
- Improve `purs.md` (literate programming tool, talk to @paf31)

## PureScript Projects

If you would like to create a project in PureScript itself, but unsure what to work on, then the community can suggest some projects which will have immediate value for users.

### FFI Bindings

FFI bindings are a good source of project ideas, since we can build on the success of existing JavaScript libraries.

- Type-safe `socket.io` wrapper for client and server
- Type-safe wrappers for HTML5 local storage APIs
- Type-safe cookie library wrappers
- Type-safe Indexed DB wrapper
- Type-safe Koa.js server wrapper
- Type-safe wrapper for [unicode-properties](https://github.com/devongovett/unicode-properties) (`isLetter`, `isLower`, `isUpper`, `isPunctuation`, ...)

### Data Structures & Algorithms

- A quaternion rotation library
- Automatic differentiation
- Interval trees (this could also reuse a lot of the code in [purescript-sequences](/hdgarrood/purescript-sequences))
- Efficient set and map union

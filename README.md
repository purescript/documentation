![PureScript](https://github.com/purescript/purescript/raw/master/logo.png)

Welcome to the PureScript documentation repository!

PureScript is a small, strongly typed programming language that compiles to JavaScript.
To get a better overview of PureScript, visit [The PureScript Website](http://purescript.org).

This repository is a collaborative effort, so please feel free to make a pull request to add/edit content or create an issue to discuss it. PureScript is a big project used by people coming from a variety of backgrounds. Making documentation useful to a wide variety of people is really hard to do well, requiring readers like you to point out and add documentation you feel is missing. Thanks for helping!

## Project Scope

Topics currently in this repository's scope:

- PureScript language reference documentation
- Its compiler errors
- Core concepts on which the language is based
- Introduction to package managers and dependency management
- Comparison with similar languages
- An introduction to other sources of documentation

Topics currently *not* in scope:

- Using PureScript libraries (those docs belong with the corresponding libraries)
- A PureScript language teaching course (use the [PureScript by Example](https://leanpub.com/purescript/read) book or other resources)

Feel free to make an issue to discuss amending the scope.

## Getting Started

- [Getting Started](guides/Getting-Started.md): Download PureScript and build your first project
- [PureScript By Example](https://leanpub.com/purescript/read): A book about PureScript. Learn functional programming for the web by solving practical problems
- [Try PureScript](http://try.purescript.org): Try PureScript in your browser

## Learning

- The [PureScript Book](https://leanpub.com/purescript/read) is the recommended approach to learning the language, since it covers more material in greater depth. However, it covers `0.11.7` and is not updated yet for the `0.12.x` version of the compiler. Thus, one should be aware of the following materials when reading through the book:
    - See [dwhitney's fork of the book's exercises](https://github.com/dwhitney/purescript-book) which is updated for `0.12.x`.
    - See [Justin's `0.11.7` to `0.12.x` summary](https://purescript-resources.readthedocs.io/en/latest/0.11.7-to-0.12.0.html) to know how to 'translate' the outdated book's code into working code.
    - Be wary of any references to these [deprecated packages](https://github.com/purescript-deprecated) in the book.
- [Language Reference](language/README.md)
- [PureScript: Jordan's Reference](https://github.com/JordanMartinez/purescript-jordans-reference): An up-to-date project covering Getting Started, Build Tools, PureScript's syntax with examples, FP design patterns, and PureScript's ecosystem.

## Guides

- [Common Operators](guides/Common-Operators.md)
- [The Foreign Function Interface (FFI)](guides/FFI.md)
- [FFI Tips](guides/FFI-Tips.md)
- [Generic Programming](guides/Generic.md)
- [Handling Native Effects with the Eff Monad](guides/Eff.md)
- [Custom Type Errors](guides/Custom-Type-Errors.md)
- [PureScript Without Node](guides/PureScript-Without-Node.md)
- [Contrib Library Guidelines](guides/Contrib-Guidelines.md)
- [Error Suggestions](guides/Error-Suggestions.md)
- [psc-ide FAQ](guides/psc-ide-FAQ.md)
- [The `Partial` type class](guides/The-Partial-type-class.md)
- [Try PureScript Help](https://github.com/purescript/trypurescript/blob/gh-pages/README.md)


## Tools

- [Editor and tool support](ecosystem/Editor-and-tool-support.md): Editor plugins, build tools, and other development tools
- [PureScript and NixOS](https://pr06lefs.wordpress.com/2015/01/11/get-started-with-purescript-on-nixos/): How to use PureScript with NixOS
- [PSCi](guides/PSCi.md): An interactive development tool for PureScript

## Ecosystem

- [Maintained Packages](ecosystem/Maintained-Packages.md)
- [Style Guide](guides/Style-Guide.md)
- [Alternate Backends](https://github.com/purescript/documentation/blob/master/ecosystem/Alternate-backends.md): PureScript can compile to other languages as well!

## Articles

- [24 Days of PureScript 2016](https://github.com/paf31/24-days-of-purescript-2016)

## Talks/Meetups

- [PureScript Presentations](ecosystem/PureScript-Presentations.md)
- [PureScript Meetups](ecosystem/PureScript-Meetups.md)

## Related Languages

- [Related Projects](Related-Projects.md)
- [Differences from Haskell](language/Differences-from-Haskell.md)

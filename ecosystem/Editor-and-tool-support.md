
## Editors

#### Atom 

- [purescript-contrib/atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting
- [nwolverson/atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides build support,   REPL, and autocomplete etc. via [psc-ide](https://github.com/purescript/purescript/tree/master/psc-ide)

#### Emacs
Install these two packages and follow the instructions in the `psc-ide-emacs` README.
- [purescript-mode](https://github.com/purescript-emacs/purescript-mode) Syntax Highlighting and indentation (adapted from haskell-mode)
- [psc-ide-emacs](https://github.com/purescript-emacs/psc-ide-emacs) Emacs plugin exposing the compilers IDE support

PSCI support via comint:

- [psci-mode](https://github.com/purescript-emacs/emacs-psci) is a REPL minor mode

Spacemacs users can just use the [`purescript` layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/purescript).

#### Sublime Text 2

- [PureScript package](https://sublime.wbond.net/search/PureScript) by joneshf

#### Vim

- [purescript-vim](https://github.com/raichoo/purescript-vim) syntax highlighting and indentation
- [FrigoEU/psc-ide-vim](https://github.com/FrigoEU/psc-ide-vim/)
- [w0rp/ale](https://github.com/w0rp/ale) supports the [purescript-language-server](https://github.com/nwolverson/purescript-language-server)

#### VS Code

- [nwolverson/vscode-ide-purescript](https://github.com/nwolverson/vscode-ide-purescript)

#### General

- To generate `TAGS` files, use `purs docs --format etags` (or `--format ctags`)

## Build tools and package managers

Maintained and actively supported tools:
- [spago](https://github.com/purescript/spago) - PureScript package manager and build tool powered by [Dhall](https://github.com/dhall-lang/dhall-lang) and [package-sets](https://github.com/purescript/package-sets)
- [psc-package](https://github.com/purescript/psc-package) - A package manager for PureScript based on the concept of package sets
- [purs-loader](https://github.com/ethul/purs-loader/) - PureScript loader for WebPack
- [pscid](https://github.com/kRITZCREEK/pscid) is a lightweight file-watcher/testrunner for PS projects providing instant-rebuilds in an editor agnostic way

Deprecated:
- [Pulp](https://github.com/purescript-contrib/pulp) - a standalone build system for PureScript ([pulp](https://www.npmjs.com/package/pulp) in `npm`). *Spago is recommended over Pulp.*
- [Gulp task](https://github.com/purescript-contrib/gulp-purescript) (`gulp-purescript` in `npm`). *Spago is recommended over the PureScript Gulp task.*

Unmaintained:
- [psvm-js](https://github.com/ThomasCrvsr/psvm-js) - PureScript Version Manager
- [psc-pane](https://github.com/anttih/psc-pane) - Auto reloading compiler which formats a single error to fit the window
- [purescript-psa](https://github.com/natefaubion/purescript-psa) - A pretty, flexible error/warning reporting frontend for `psc` featuring colours, original source spans in errors, warning filtering and persistence.

# Editor and tool support

The PureScript ecosystem contains a number of editor plugins and tools to help you build libraries and applications. The set of available tools is growing continuously, but this documentation contains popular, stable tools that you can use today.

At minimum, almost all projects will use:

- The [PureScript compiler](https://github.com/purescript/purescript)
- The [Spago legacy](https://github.com/purescript/spago-legacy) build tool and package manager
- The [Spago rewrite](https://github.com/purescript/spago), a recent major rewrite of Spago package manager

These tools can be installed via [NPM](https://www.npmjs.com), among other installation methods.

#### Which Spago?

At the moment of writing, Spago is going through a period of transition from an older to a new codebase. Consider the following characteristics of each:

**Spago**
- The currently maintained and actively developed version of Spago
- Considered to be in its alpha stage

**Spago legacy**
- Officially deprecated and not maintained, except for security updates
- Considered to be stable
- A lot of PureScript documentation and guides released pre-2024 would link to the `purescript/spago` repository while actually having Spago legacy and its API in mind
- Most PureScript libraries and projects will be using this version for a while

Note that both versions are extensively documented in their respective repositories, consult these if you face any issues. Spago rewrite offers automatic ways of migration from Spago legacy-driven projects.

## Editor support

The PureScript compiler includes an IDE server, [`purs ide`](https://github.com/purescript/purescript/tree/master/psc-ide), to supply tooling for editors. This server has been used to implement a [PureScript language server](https://github.com/nwolverson/purescript-language-server), which implements the [Language Server Protocol](https://langserver.org) for PureScript. Major editor tooling either uses `purs ide server` directly or via the language server.

Most editor plugins which rely on PureScript's IDE tooling have at least these features:

- Autocomplete (including auto-imports)
- Definitions and error reporting on hover
- Go-to-definition and local search
- REPL support
- Automatic builds
- Error suggestions and quick-fix actions for missing type signatures, imports, and more
- Case split for type-driven development

### General

Some tools are commonly used with several editors, as they are implemented to be editor-agnostic:

- [psa](https://github.com/natefaubion/purescript-psa) is a pretty, flexible error/warning reporting frontend for the compiler featuring colours, original source spans in errors, warning filtering and persistence.
- [pscid](https://github.com/kRITZCREEK/pscid) is a lightweight file-watcher and test runner which provides instant-rebuilds in an editor agnostic way.

To generate `TAGS` files, use `purs docs --format etags` (or `--format ctags`).

### Atom

- [atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting for `.purs` files.
- [atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides editor support via `purs ide`.

### Emacs

- [psc-ide-emacs](https://github.com/purescript-emacs/psc-ide-emacs) provides editor support via `purs ide`.
- [purescript-mode](https://github.com/purescript-emacs/purescript-mode) provides syntax highlighting and indentation rules for `.purs` files.
- [psci-mode](https://github.com/purescript-emacs/emacs-psci) provides a minor mode for a PureScript REPL.

Spacemacs users can use the [PureScript layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/purescript).

### IntelliJ

- [PureScript plugin](https://plugins.jetbrains.com/plugin/9738-purescript) provides editor support via `purs ide`, syntax highlighting for `.purs` files, and much more.

### Sublime Text 3

- [purescript-ide-sublime](https://packagecontrol.io/packages/PureScript) provides editor support via `purs ide` and syntax highlighting for `.purs` files.

### Vim

- [psc-ide-vim](https://github.com/FrigoEU/psc-ide-vim) provides editor support via `purs ide`.
- [ale](https://github.com/dense-analysis/ale) provides editor support via the PureScript language server.
- [coc.nvim](https://github.com/neoclide/coc.nvim) provides editor support via the PureScript language server.
- [purescript-vim](https://github.com/raichoo/purescript-vim) provides syntax highlighting and indentation rules for `.purs` files.
- [vim-lsp-settings](https://github.com/mattn/vim-lsp-settings) provides automatic configuration for editor support via the PureScript language server

### VSCode

- [ide-purescript](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) provides editor support via the PureScript language server.
- [language-purescript](https://marketplace.visualstudio.com/items?itemName=nwolverson.language-purescript) provides syntax highlighting for `.purs` files.

## Tooling support

There are many tools available to help you develop libraries and applications in PureScript. These include build tools, package managers, code formatters, dead code elimination tools, GitHub Actions, Nix tools, and more.

### Build tools and package managers

There are several build tools and package managers available for PureScript.

- [spago](https://github.com/purescript/spago) is the standard package manager and build tool for Purescript.

These build tools are maintained, but are no longer recommended for most projects (use Spago instead):

- [psc-package](https://github.com/purescript/psc-package) is a package manager for PureScript based on [package-sets](https://github.com/purescript/package-sets) and a precursor to Spago.
- [pulp](https://github.com/purescript-contrib/pulp) is a standalone build system for PureScript which relies on [Bower](https://github.com/bower/bower) for package management and a precursor to Spago.

### Development tools

- [purescript-backend-optimizer](https://github.com/aristanetworks/purescript-backend-optimizer) is an optimizing backend toolkit for PureScript's CoreFn and a code-generator which outputs modern ECMAScript.
- [setup-purescript](https://github.com/purescript-contrib/setup-purescript) is a GitHub Action which sets up a PureScript toolchain with common tools including the compiler and Spago for continuous integration.
- [purs-tidy](https://github.com/natefaubion/purescript-tidy) is a formatter and pretty-printer for PureScript code.
- [zephyr](https://github.com/coot/zephyr) is a dead code elimination tool for PureScript applications which can be used to reduce bundle sizes.

#### For Nix users

There are some tools meant specifically for Nix users, who may not be able to use installation methods like NPM.

- [easy-purescript-nix](https://github.com/justinwoo/easy-purescript-nix) provides many common tools in the PureScript community like the compiler (`purs`), `spago`, `pscid`, `zephyr`, and more for Nix environments.
- [purs-nix](https://github.com/purs-nix/purs-nix) provides support for official package-set, namespaces and overlays on Nix without a external package manager (like spago).
- [purifix](https://github.com/purifix/purifix) package manager in nix using spago (spago.yaml) as single source of truth.
- [spago2nix](https://github.com/justinwoo/spago2nix) helps you generate Nix expressions for your Spago dependencies.
- [yarn2nix](https://github.com/nix-community/yarn2nix) helps you generate Nix expressions for your JavaScript dependencies.

### Backend-specific tooling

PureScript projects which use alternate backends may use tools from the ecosystem of the target language. This documentation is a non-exhaustive starting point for some tools you may find useful when working with particular backends for PureScript.

#### JavaScript

PureScript projects which target JavaScript may find some tools from the JavaScript ecosystem. PureScript code which imports libraries from JavaScript will at least require a JavaScript package manager (to install dependencies) and a JavaScript bundling tool (to resolve JavaScript imports, among other things). You may also want to use a linter for any JavaScript code you are writing via the FFI.

- Some popular package managers in JavaScript include [npm](https://www.npmjs.com) (recommended), [yarn](https://yarnpkg.com), and [pnpm](https://pnpm.js.org).
- The recommended way to bundle PureScript code is by using Spago's `spago bundle` command, which uses [esbuild](https://esbuild.github.io/). Other popular bundlers include [webpack](https://webpack.js.org) with the [purs-loader](https://github.com/ethul/purs-loader) PureScript loader, and [parcel](https://parceljs.org).
- Some popular linters include [eslint](https://eslint.org) and [jsconfig](https://code.visualstudio.com/docs/languages/jsconfig).

#### Erlang

- [purerlex](https://github.com/drathier/purerlex) integrates Purerl code compilation process with BEAM platform's Mix project management tool.

### Deprecated and unmaintained tools

These tools were previously mentioned in this documentation, but are no longer maintained or recommended:

- [purty](https://gitlab.com/joneshf/purty) was a PureScript code formatter and pretty-printer.
- [gulp-purescript](https://github.com/purescript-deprecated/gulp-purescript) was a Gulp task for Purescript.
- [psvm-js](https://github.com/ThomasCrvsr/psvm-js) was a version manager (like `nvm` for Node) for PureScript compiler versions.
- [psc-pane](https://github.com/anttih/psc-pane) provided auto-reloading builds which formatted a single error to fit the window.

# Editor and tool support

The PureScript ecosystem contains a number of editor plugins and tools to help you build libraries and applications. The set of available tools is growing continuously, but this documentation contains popular, stable tools that you can use today.

At minimum, almost all projects will use:

- The [PureScript compiler](https://github.com/purescript/purescript)
- The [Spago](https://github.com/purescript/spago) build tool and package manager

These tools can be installed via [NPM](https://www.npmjs.com), among other installation methods.

## Editor support

The PureScript compiler includes an IDE server, [`psc-ide`](https://github.com/purescript/purescript/tree/master/psc-ide), to supply tooling for editors. Most editor plugins which rely on PureScript's IDE tooling have at least these features:

- Autocomplete (including auto-imports)
- Definitions and error reporting on hover
- Go-to-definition and local search
- REPL support
- Automatic builds
- Error suggestions and quick-fix actions for missing type signatures, imports, and more
- Case split for type-driven development

### General

Some tools are commonly used with several editors, as they are implemented to be editor-agnostic:

- [purescript-language-server](https://github.com/nwolverson/purescript-language-server) implements the [Language Server Protocol](https://langserver.org) for PureScript via `psc-ide`. Almost all editor tooling either uses `psc-ide` or the language server.
- [psa](https://github.com/natefaubion/purescript-psa) is a pretty, flexible error/warning reporting frontend for the compiler featuring colours, original source spans in errors, warning filtering and persistence.
- [pscid](https://github.com/kRITZCREEK/pscid) is a lightweight file-watcher and test runner which provides instant-rebuilds in an editor agnostic way.

To generate `TAGS` files, use `purs docs --format etags` (or `--format ctags`).

### Atom

- [atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting for `.purs` files.
- [atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides editor support via `psc-ide`.

### Emacs

- [psc-ide-emacs](https://github.com/purescript-emacs/psc-ide-emacs) provides editor support via `psc-ide`.
- [purescript-mode](https://github.com/purescript-emacs/purescript-mode) provides syntax highlighting and indentation rules for `.purs` files.
- [psci-mode](https://github.com/purescript-emacs/emacs-psci) provides a minor mode for a PureScript REPL.

Spacemacs users can use the [PureScript layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/purescript).

### Sublime Text 3

- [purescript-ide-sublime](https://packagecontrol.io/packages/PureScript) provides editor support via `psc-ide` and syntax highlighting for `.purs` files.

### Vim

- [psc-ide-vim](https://github.com/FrigoEU/psc-ide-vim) provides editor support via `psc-ide`.
- [ale](https://github.com/dense-analysis/ale) provides editor support via the PureScript language server.
- [coc.nvim](https://github.com/neoclide/coc.nvim) provides editor support via the PureScript language server.
- [purescript-vim](https://github.com/raichoo/purescript-vim) provides syntax highlighting and indentation rules for `.purs` files.

### VSCode

- [ide-purescript](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) provides editor support via the PureScript language server.
- [language-purescript](https://marketplace.visualstudio.com/items?itemName=nwolverson.language-purescript) provides syntax highlighting for `.purs` files.

## Tooling support

There are many tools available to help you develop libraries and applications in PureScript. These include build tools, package managers, code formatters, dead code elimination tools, GitHub Actions, Nix tools, and more.

### Build tools and package managers

There are several build tools and package managers available for PureScript.

- [spago](https://github.com/purescript/spago) is the standard package manager and build tool for Purescript, powered by [Dhall](https://github.com/dhall-lang/dhall-lang) and [package-sets](https://github.com/purescript/package-sets).

These build tools are maintained, but are no longer recommended for most projects (use Spago instead):

- [psc-package](https://github.com/purescript/psc-package) is a package manager for PureScript based on [package-sets](https://github.com/purescript/package-sets) and a precursor to Spago.
- [pulp](https://github.com/purescript-contrib/pulp) is a standalone build system for PureScript which relies on [Bower](https://github.com/bower/bower) for package management and a precursor to Spago.

### Development tools

- [setup-purescript](https://github.com/purescript-contrib/setup-purescript) is a GitHub Action which sets up a PureScript toolchain with common tools including the compiler and Spago for continuous integration.
- [purty](https://gitlab.com/joneshf/purty) is a formatter / pretty-printer for PureScript source code.
- [zephyr](https://github.com/coot/zephyr) is a dead code elimination tool for PureScript applications which can be used to reduce bundle sizes.

#### For Nix users

There are some tools meant specifically for Nix users, who may not be able to use installation methods like NPM.

- [easy-purescript-nix](https://github.com/justinwoo/easy-purescript-nix) provides many common tools in the PureScript community like the compiler (`purs`), `spago`, `pscid`, `zephyr`, and more for Nix environments.
- [spago2nix](https://github.com/justinwoo/spago2nix) helps you generate Nix expressions for your Spago dependencies.
- [yarn2nix](https://github.com/nix-community/yarn2nix) helps you generate Nix expressions for your JavaScript dependencies.

### Backend-specific tooling

PureScript projects which use alternate backends may use tools from the ecosystem of the target language. This documentation is a non-exhaustive starting point for some tools you may find useful when working with particular backends for PureScript.

#### JavaScript

PureScript projects which target JavaScript may find some tools from the JavaScript ecosystem. PureScript code which imports libraries from JavaScript will at least require a JavaScript package manager (to install dependencies) and a JavaScript bundling tool (to resolve JavaScript imports, among other things). You may also want to use a linter for any JavaScript code you are writing via the FFI.

- Some popular package managers in JavaScript include [npm](https://www.npmjs.com) (recommended), [yarn](https://yarnpkg.com), and [pnpm](https://pnpm.js.org).
- Some popular bundlers in JavaScript include [webpack](https://webpack.js.org) (recommended) with the [purs-loader](https://github.com/ethul/purs-loader) PureScript loader, and [parcel](https://parceljs.org). Other JavaScript bundlers like [rollup](https://github.com/rollup/rollup) rely on ES Modules, which PureScript does not yet support.
- Some popular linters include [eslint](https://eslint.org) and [jsconfig](https://code.visualstudio.com/docs/languages/jsconfig).

### Deprecated and unmaintained tools

These tools were previously mentioned in this documentation, but are no longer maintained or recommended:

- [gulp-purescript](https://github.com/purescript-deprecated/gulp-purescript) was a Gulp task for Purescript`npm`).
- [psvm-js](https://github.com/ThomasCrvsr/psvm-js) was a version manager (like `nvm` for Node) for PureScript compiler versions.
- [psc-pane](https://github.com/anttih/psc-pane) provided auto-reloading builds which formatted a single error to fit the window.

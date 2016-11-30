## Editors

- Atom 
  - [purescript-contrib/atom-language-purescript](https://github.com/purescript-contrib/atom-language-purescript) provides syntax highlighting
  - [nwolverson/atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript) provides build support,   REPL, and autocomplete etc. via [psc-ide](https://github.com/purescript/purescript/tree/master/psc-ide-server)
- Emacs
  - [dysinger/purescript-mode](https://github.com/dysinger/purescript-mode) was adapted from haskell-mode
  - [epost/psc-ide-emacs](https://github.com/epost/psc-ide-emacs) offers Emacs support for [psc-ide](https://github.com/purescript/purescript/tree/master/psc-ide-server)
  - [emacs-pe/purescript-mode](https://github.com/emacs-pe/purescript-mode) is an alpha-stage greenfield mode
  - [ardumont/psci-mode](https://github.com/ardumont/emacs-psci) is a REPL minor mode
  - [spion/purscheck](https://github.com/spion/purscheck) provides flycheck support.
  - [emacs-pe/flycheck-purescript](https://github.com/emacs-pe/flycheck-purescript) provides flycheck support.

- Sublime Text 2 - [PureScript package](https://sublime.wbond.net/search/PureScript) by joneshf
- Vim
  - [purescript-vim](https://github.com/raichoo/purescript-vim) syntax highlighting and indentation
  - [FrigoEU/psc-ide-vim](https://github.com/FrigoEU/psc-ide-vim/)
- IntelliJ IDEA - [ikarienator/pure-idea](https://github.com/ikarienator/pure-idea)
- Visual Studio - [nwolverson/vscode-ide-purescript](https://github.com/nwolverson/vscode-ide-purescript)
- General
  - [kRITZCREEK/psc-ide](https://github.com/kRITZCREEK/psc-ide) (bundled with the compiler from version 0.8.2) is a client and server that provides  generic editor support
  - To generate `TAGS` files, use `psc-docs --format etags` (or `--format ctags`)

## Build tools

- [Pulp](https://github.com/bodil/pulp) - a standalone build system for PureScript ([pulp](https://www.npmjs.com/package/pulp) in `npm`)
- [Grunt task](https://github.com/purescript-contrib/grunt-purescript) (`grunt-purescript` in `npm`) (deprecated)
- [Gulp task](https://github.com/purescript-contrib/gulp-purescript) (`gulp-purescript` in `npm`)
- [psvm-js](https://github.com/ThomasCrvsr/psvm-js) - PureScript Version Manager
- [purs-loader](https://github.com/ethul/purs-loader/) - PureScript loader for WebPack
- [psc-pane](https://github.com/anttih/psc-pane) - Auto reloading compiler which formats a single error to fit the window
- [purescript-psa](https://github.com/natefaubion/purescript-psa) - A pretty, flexible error/warning reporting frontend for `psc` featuring colours, original source spans in errors, warning filtering and persistence.
- [pscid](https://github.com/kRITZCREEK/pscid) is a lightweight file-watcher/testrunner for PS projects, that uses `psc-ide` to provide fast rebuilds.

## Wish List

- Type at the cursor
- Generate minimal imports
- Go to definition 
- Go to documentation (open Pursuit)
- Build, then highlight/jump to errors
- Answer: "what can I put in here to make it type check?"
- What options do I have for importing this symbol? Sort by which ones will make the code type check
- Qualify implicit imports
- Turn implicit imports into explicit ones
- Features work or at least are usable using any text editor and psc from the console
- Find all references (for refactoring / renaming)
- Fix `TransitiveExportError` ("An export of X requires Y to also be exported") automatically

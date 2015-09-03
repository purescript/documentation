## Editors

- [Atom](https://github.com/darinmorrison/atom-language-purescript)
- Emacs
  - [dysinger/purescript-mode](https://github.com/dysinger/purescript-mode) was adapted from haskell-mode
  - [emacs-pe/purescript-mode](https://github.com/emacs-pe/purescript-mode) is an alpha-stage greenfield mode
  - [ardumont/psci-mode](https://github.com/ardumont/emacs-psci) is a REPL minor mode
  - [spion/purscheck](https://github.com/spion/purscheck) provides flycheck support.
  - [epost/psc-ide-emacs](https://github.com/epost/psc-ide-emacs) is an alpha stage Emacs binding for [kRITZCREEK/psc-ide](https://github.com/kRITZCREEK/psc-ide)
- [Sublime Text 2](https://sublime.wbond.net/search/PureScript)
- [Vim](https://github.com/raichoo/purescript-vim)
- [IntelliJ IDEA](https://github.com/ikarienator/pure-idea)
- General
  - [kRITZCREEK/psc-ide](https://github.com/kRITZCREEK/psc-ide) is a client and server that provides  generic editor support
  - To generate `TAGS` files, use `psc-docs --format etags` (or `--format ctags`)

## Build tools

- [Pulp](https://github.com/bodil/pulp) - a standalone build system for PureScript ([pulp](https://www.npmjs.com/package/pulp) in `npm`)
- [Grunt task](https://github.com/purescript-contrib/grunt-purescript) (`grunt-purescript` in `npm`)
- [Gulp task](https://github.com/purescript-contrib/gulp-purescript) (`gulp-purescript` in `npm`)
- [psvm-js](https://github.com/ThomasCrvsr/psvm-js) - PureScript Version Manager

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
# Frequently asked `psc-ide` questions

## Q: Where can I get psc-ide?
A: From compiler version > 0.8.3 onwards psc-ide is distributed with the compiler and will be installed with the compiler binaries.

## Q: How do I get psc-ide support for my editor?
A: If you are using one of Atom, Emacs, VIM or VS Code:

- Atom:

  1. install the `language-purescript` package
  2. install the `ide-purescript` package

- Emacs:

  install `psc-ide` from MELPA

- (Spacemacs):

  enable the purescript layer

- VIM:

  `NeoBundle "frigoeu/psc-ide-vim"`

- VS Code:

  install the ide-purescript package

For any other editor... you'll have to write your own plugin :smile:

## Q: Where can I get documentation for psc-ide?
A: https://github.com/purescript/purescript/tree/master/psc-ide-server

# Troubleshooting

## Parsing the externs failed...

Step 1: Delete your `output/` folder and recompile your project

Step 2: Check if your `psc --version` and `psc-ide-server --version` match

Step 3: Check if you have an old psc-ide lying around (think stack, npm or cabal)

Step 4: Hop onto IRC and start a debugging session with @kritzcreek
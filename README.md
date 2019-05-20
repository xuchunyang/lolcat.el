# Emacs Port of LOLcat

[![Build Status](https://travis-ci.org/xuchunyang/lolcat.el.svg?branch=master)](https://travis-ci.org/xuchunyang/lolcat.el)

The original [LOLcat](https://github.com/busyloop/lolcat) is written in Ruby, but this Emacs version is ported from the [Python version](https://github.com/tehmaze/lolcat).

## Screenshot

Running `$ lolcat lolcat.el` in Eshell:

![screenshot of running lolcat in eshell](lolcat.png)

Unfortunately, Eshell doesn't support `STDIN`, so `$ fortune | cowsay | lolcat` won't work in Eshell.

## Usage

### `M-x lolcat-view-file FILE`

Mainly for testing.

### `eshell/lolcat`

This function allows you to use `$ lolcat FILENAME` in Eshell.

### `(lolcat s &optional seed freq spread)`

This function is indented to be used in your Lisp code, it adds text properties to the string `S` for coloring.

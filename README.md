# Emacs Port of LOLcat

[![Build Status](https://travis-ci.org/xuchunyang/lolcat.el.svg?branch=master)](https://travis-ci.org/xuchunyang/lolcat.el)

The original [LOLcat](https://github.com/busyloop/lolcat) is written in Ruby. This Emacs version is ported from the [Python version](https://github.com/tehmaze/lolcat).

## Screenshot

Running `$ lolcat lolcat.el` in Eshell:

![screenshot of running lolcat in eshell](lolcat.png)

## Usage

### `M-x lolcat-view-file FILE`

View `FILE` in color.

### `M-x lolcat-view-buffer BUFFER`

View `BUFFER` in color.

### `M-x lolcat-this-buffer`

Colorize the current buffer.

### `eshell/lolcat`

The Eshell command.

_Unfortunately, Eshell doesn't support `STDIN`, so `$ fortune | cowsay | lolcat` won't work in Eshell._

### `(lolcat s &optional seed freq spread)`

The public API.

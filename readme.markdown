# idris-mode for emacs

This is an emacs mode for editing [Idris][] code.

Syntax highlighting functional languages is a little quirky; I've
deferred to haskell-mode for some things
(e.g. `font-lock-variable-name-face` for operators) and done some
other things as appropriate for Idris. `font-lock-type-face` isn't
used at all, for example, and data types declared with `data` use the
same face as functions and values defined with `=`.

![Screenshot](http://itu.dk/~hame/idris-emacs.png)

## Documentation

There are some docstrings spread around the mode, thus C-h m is helpful and returns the available key bindings.
The REPL also supports tab completion, thus pressing tab opens a buffer with the available completions.

Some preliminary notes are available in the [pdf]

[pdf]: http://itu.dk/~hame/idris-mode.pdf

## Inferior Idris

There is now support for running an Idris interpreter in a buffer. Use
'C-c C-l' to load the current Idris buffer into the interpreter. This will
spawn an inferior idris process and load the buffer. It will report warnings
if idris reports any. Pressing C-c C-l again will reload that buffer - if you
switch to a different buffer and press C-c C-l, that buffer will be loaded
instead.

Customize `inferior-idris-path` if idris is not on your default path.

[Idris]: http://www.idris-lang.org

## Installation

If you have Emacs >= 23, you can install pre-built packages from
[MELPA](http://melpa.milkbox.net/): `idris-mode` will automatically be enabled
in `.idr` files without any further configuration.

Alternatively, download the elisp files, place them somewhere in your load
path and `(require 'idris-mode)` somewhere in `~/.emacs` or `~.emacs.d/init.el`.

## Customization

Customize various aspects of the mode using `M-x customize-group RET idris RET`.


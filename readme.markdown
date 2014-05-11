![Interactive editing](http://itu.dk/people/drc/idris-mode.gif)

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

The [extended abstract] for DTP14 gives overview of the features of idris-mode. Some preliminary notes are available in the [pdf]

[extended abstract]: http://itu.dk/people/drc/pubs/dtp2014-idris-mode.pdf
[pdf]: http://itu.dk/people/hame/idris-mode.pdf

## Inferior Idris

There is now support for running an Idris interpreter in a buffer. Use
'C-c C-l' to load the current Idris buffer into the interpreter. This will
spawn an inferior idris process and load the buffer. It will report warnings
if idris reports any. Pressing C-c C-l again will reload that buffer - if you
switch to a different buffer and press C-c C-l, that buffer will be loaded
instead.

Customize `inferior-idris-path` if idris is not on your default path.

[Idris]: http://www.idris-lang.org

## Colors and fonts
Idris mode displays output from the Idris compiler with full semantic highlighting. It is quite possible that this is ugly in your color scheme. If that is the case, you can use `M-x customize-group RET idris-faces RET` to modify them.

## Interactive Editing

![Interactive editing](http://itu.dk/people/drc/idris-mode.gif)

The following commands are available when there is an inferior Idris process (which is started on demand by the commands):

* `C-c C-l`: Load the current file into Idris. A prefix version does not switch to the REPL buffer afterwards.
* `C-c C-s`: Create an initial pattern match clause for a type declaration
* `C-c C-m`: Add missing pattern-match cases to an existing definition
* `C-c C-a`: Attempt to solve a metavariable automatically. A prefix argument prompts for hints.
* `C-c C-c`: Case split the pattern variable under point
* `C-c C-t`: Get the type for the identifier under point. A prefix argument prompts for the name.
* `C-c C-d`: Get the documentation for the identifier under point. A prefix argument prompts for the name.
* `C-c C-w`: Add a with block for the pattern-match clause under point
* `C-c C-h a`: Search names, types, and docstrings for a given string.
* `C-c C-z`: Pop to a presently open REPL buffer

## Completion

`M-Tab` or whatever you have `completion-at-point` bound to will ask the running Idris process for completions for the current identifier. Note that this command requires that the Idris interpreter is already running, because attempting to load an incomplete buffer would probably not work.

## Package files
Idris's build system, which consists of package files ending in `.ipkg`, has rudimentary support from `idris-mode`. The following commands are available in Idris buffers or package buffers; if they are run from an Idris buffer, then `idris-mode` will attempt to locate the package file automatically.
* `C-c c`: Clean the package, removing `.ibc` files
* `C-c b`: Build the package
* `C-c i`: Install the package to the user's repository, building first if necessary

The following commands are available in `idris-ipkg-mode`:
* `C-c C-f`: Insert a field, with completion support. Completion for field names is also available by pressing `M-TAB`.

Additionally, the command `M-x idris-start-project` will create a directory structure and initial package file for a new project.

## Installation

If you have Emacs >= 23, you can install pre-built packages from
[MELPA](http://melpa.milkbox.net/): `idris-mode` will automatically be enabled
in `.idr` files without any further configuration.

Alternatively, download the elisp files, and place them somewhere in your load
path.

If you want `idris-mode` to be enabled by default, add the line `(require 'idris-mode)` to your `~/.emacs` or `~/.emacs.d/init.el` file.

Idris mode is heavily dependent on the Idris compiler for its more advanced features. Thus, please ensure that Emacs can see your Idris binary. Emacs looks for executables in the directories specified in the variable `exec-path`, which is initialized from your PATH at startup. If Idris is not on your PATH, then you may need to add it to `exec-path` manually. E.g.: if you installed idris with cabal into `~/.cabal/bin`, then add the line `(add-to-list 'exec-path "~/.cabal/bin")` to your emacs initialization file. Alternatively, you can customize the variable `idris-interpreter-path` and provide an absolute path.

## Customization

Customize various aspects of the mode using `M-x customize-group RET idris RET`.


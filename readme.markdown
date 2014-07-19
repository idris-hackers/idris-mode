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

Certain areas of `idris-mode` show explanatory help text. When you've learned how to use `idris-mode`, you can turn these off by setting `idris-show-help-text` to `nil`.

## Inferior Idris

There is now support for running an Idris interpreter in a buffer. Use
'C-c C-l' to load the current Idris buffer into the interpreter. This will
spawn an inferior idris process and load the buffer. It will report warnings
if idris reports any. Pressing C-c C-l again will reload that buffer - if you
switch to a different buffer and press C-c C-l, that buffer will be loaded
instead.

Customize `inferior-idris-path` if idris is not on your default path.

[Idris]: http://www.idris-lang.org

## Keybindings
`idris-mode` follows conventions from SLIME whenever possible. In particular:

* For ease of typing, any sequence of three keys allows the final key to be pressed with or without the control key, unless the last key is `h`. For example, `C-c C-d d` can also be typed `C-c C-d C-d`.
* Documentation-related commands are prefixed with `C-c C-d`.
* Commands related to **b**uilding packages are prefixed with `C-c C-b`.

## Error messages

When loading a buffer, `idris-mode` will decorate errors from the Idris compiler with underlines. Tooltips show the error message.
The following error message commands are available:

* `M-n`: Move the point to the next compiler note
* `M-p`: Move the point to the previous comiler note

## Colors and fonts
Idris mode displays output from the Idris compiler with full semantic highlighting. It is quite possible that this is ugly in your color scheme. If that is the case, you can use `M-x customize-group RET idris-faces RET` to modify them. In particular, some users don't like the background color for the currently loaded region of the buffer. This is controlled by `idris-loaded-region-face`. Remove all it's properties to make it disappear.

## Interactive editing

![Interactive editing](http://itu.dk/people/drc/idris-mode.gif)

The following commands are available when there is an inferior Idris process (which is started on demand by the commands):

* `C-c C-l`: Load the current file into Idris. A prefix argument causes Idris to only load the portion of the file up to point.
* `C-u C-u C-c C-l`: Obliterate the loading marker, switching back to loading the whole buffer.
* `C-c C-n`, `C-c C-p`: Extend and contract the region to be loaded, if such a region exists, one line at a time.
* `C-c C-s`: Create an initial pattern match clause for a type declaration
* `C-c C-m`: Add missing pattern-match cases to an existing definition
* `C-c C-a`: Attempt to solve a metavariable automatically. A plain prefix argument prompts for hints, while a numeric prefix argument sets the recursion depth.
* `C-c C-e`: Extract a metavariable or provisional definition name to an explicit top level definition
* `C-c C-c`: Case split the pattern variable under point
* `C-c C-t`: Get the type for the identifier under point. A prefix argument prompts for the name.
* `C-c C-w`: Add a with block for the pattern-match clause under point
* `C-c C-h a`: Search names, types, and docstrings for a given string.
* `C-c C-z`: Pop to a presently open REPL buffer

## Online documentation

The Idris compiler supports documentation. The following commands access it:
* `C-c C-d d`: Show the documentation for the name under point (`:doc` at the REPL). A prefix argument prompts for the name.
* `C-c C-d a`: Search the documentation for a string (`:apropos` at the REPL).
* `C-c C-d t`: Search for documentation regarding a particular type (`:search` at the REPL).

## Completion

`M-Tab` or whatever you have `completion-at-point` bound to will ask the running Idris process for completions for the current identifier. Note that this command requires that the Idris interpreter is already running, because attempting to load an incomplete buffer would probably not work.

## Active terms

Some terms output by the Idris compiler are *active*, meaning that `idris-mode` is aware of their original representation. For these terms, commands exist to normalise them and show or hide their implicit arguments.

To see the active terms available, use the command `idris-add-term-widgets`, which is also found in the menu. To issue term commands, right-click on the triangle that points at the term. The widgets can be removed again using `idris-remove-term-widgets`.

The following keybindings are available:
* `C-c C-m n`: Normalize the term at point (`M-x idris-normalize-term`)
* `C-c C-m i`: Show implicits for the term at point (`M-x idris-show-term-implicits`)
* `C-c C-m h`: Hide implicits for the term at point (`M-x idris-hide-term-implicits`)

## Package files
Idris's build system, which consists of package files ending in `.ipkg`, has rudimentary support from `idris-mode`. The following commands are available in Idris buffers or package buffers; if they are run from an Idris buffer, then `idris-mode` will attempt to locate the package file automatically. The mnemonic for `C-b` in the prefix is "build".
* `C-c C-b c`: Clean the package, removing `.ibc` files
* `C-c C-b b`: Build the package
* `C-c C-b i`: Install the package to the user's repository, building first if necessary

The following commands are available in `idris-ipkg-mode`:
* `C-c C-f`: Insert a field, with completion support. Completion for field names is also available by pressing `M-TAB`.

When a package is present, `idris-mode` gains a few convenience features. In particular, the Idris compiler's working directory is set based on the `sourcedir` directive in the package file, and certain filenames or module names become clickable buttons, to conveniently open them.

Additionally, the command `M-x idris-start-project` will create a directory structure and initial package file for a new project.

## Using packages
`idris-mode` supports setting the packages to be loaded by the Idris process. Specifically, the buffer-local variable `idris-packages` is expected to contain a list of package names to be loaded. When the buffer is loaded, if the current packages loaded by the Idris process don't agree with the contents of the variable, Idris is restarted with the correct `-p` options.

You can set this variable interactively using the command `M-x idris-set-idris-packages`. This will add the variable as a file-local variable, so that it will be set automatically when the file is loaded in the future.

## Installation

Idris mode uses lexical binding and other features not available in versions of Emacs prior to 24. Thus, only Emacs 24 and up are supported.

You can install pre-built packages from
[MELPA](http://melpa.milkbox.net/): `idris-mode` will automatically be enabled
in `.idr` files without any further configuration.

Alternatively, download the elisp files, and place them somewhere in your load
path.

If you want `idris-mode` to be enabled by default, add the line `(require 'idris-mode)` to your `~/.emacs` or `~/.emacs.d/init.el` file.

Idris mode is heavily dependent on the Idris compiler for its more advanced features. Thus, please ensure that Emacs can see your Idris binary. Emacs looks for executables in the directories specified in the variable `exec-path`, which is initialized from your PATH at startup. If Idris is not on your PATH, then you may need to add it to `exec-path` manually. E.g.: if you installed idris with cabal into `~/.cabal/bin`, then add the line `(add-to-list 'exec-path "~/.cabal/bin")` to your emacs initialization file. Alternatively, you can customize the variable `idris-interpreter-path` and provide an absolute path.

## Customization

Customize various aspects of the mode using `M-x customize-group RET idris RET`.

Additionally, you may want to update your Emacs configuration so that it does not open Idris bytecode files by default. You can do this by adding `".ibc"` to the variable `completion-ignored-extensions`, either in customize or by adding `(add-to-list 'completion-ignored-extensions ".ibc")` to your `init.el`. If you use `ido`, then you may also need to set `ido-ignore-extensions` to `t`.

## Keybinding conventions

All three-letter keybindings are available in versions with and without `C-` on the final key, following the convention from SLIME.

## Evil mode support (Vim compatability)
There is emulation for idris-vim commands in idris-mode. To enable this support please install the `evil` and `evil-leader` packages from MELPA (or your favorite source of packages).

The following commands are supported (taken from idris-vim):

* `<LocalLeader>r`: Reload file

* `<LocalLeader>t`: Show type

* `<LocalLeader>d`: Add initial pattern-match clause

* `<LocalLeader>c`: Case split

* `<LocalLeader>w`: Add `with` clause

* `<LocalLeader>m`: Add missing pattern-match cases

* `<LocalLeader>p`: Proof search

* `<LocalLeader>h`: Show documentation

## Tests

Before sending a patch or pull request, please run the automated tests for idris-mode and correct any errors that are found. There are two kinds of test:

1. The Emacs byte code compiler can catch many issues. Running `make compile` will invode the byte code compiler, failing if there are any warnings. You may wish to run `make clean` after `make compile` to get rid of pesky `.elc` files.

2. There is a test suite that can be invoked with `make test`. It requires a functioning `idris` executable.


## Related packages

[`helm-idris`](https://www.github.com/david-christiansen/helm-idris) builds on `idris-mode` to provide an alternative interface to looking up documentation.


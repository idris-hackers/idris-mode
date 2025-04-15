# Changes

This file documents the user-interface changes in idris-mode, starting
with release 0.9.19.

## master (unreleased)

### New features

+ [cab781537f](https://github.com/idris-hackers/idris-mode/commit/cab781537f): Improved flycheck integration for Idris1 and Idris2.
+ [c9b2a4bee6](https://github.com/idris-hackers/idris-mode/commit/c9b2a4bee6): Xref integration to support "jump to definition" features.
+ [103f1e5fbf](https://github.com/idris-hackers/idris-mode/commit/103f1e5fbf): New command `M-x idris-switch-to-last-idris-buffer` to move point from Idris repl buffer to Idris source code buffer.
  It is opposite of `M-x idris-switch-to-repl` and uses same key binding by default (`C-c C-z`).
+ [e350ed25a5](https://github.com/idris-hackers/idris-mode/commit/e350ed25a5): New command `idris-compile-and-execute`. Backport of `idris2-compile-and-execute` from https://github.com/idris-community/idris2-mode/pull/20/files with preserving backward compatibility for Idris 1.
+ [e350ed25a5](https://github.com/idris-hackers/idris-mode/commit/e350ed25a5): New command `idris-intro`. Backport of `idris2-intro from` https://github.com/idris-community/idris2-mode/pull/21/files
+ [cc098578fe](https://github.com/idris-hackers/idris-mode/commit/cc098578fe): Restore position after case split with improved user experience. Related to https://github.com/idris-hackers/idris-mode/pull/465
+ [3cce2336b7](https://github.com/idris-hackers/idris-mode/commit/3cce2336b7): More granular configuration for enabling semantic source highlighting.

### Changes

+ [b6a5b2ec60](https://github.com/idris-hackers/idris-mode/commit/b6a5b2ec60): Kill Idris buffer and it's window if it was the only buffer in windows history.
+ [d1a9171fd7](https://github.com/idris-hackers/idris-mode/commit/d1a9171fd7): Jump to last Idris Code buffer when we quit buffer
+ [cd734fdc7a](https://github.com/idris-hackers/idris-mode/commit/cd734fdc7a): Write Idris repl history file to `~/.idris2/` directory.
+ [8329b73be8](https://github.com/idris-hackers/idris-mode/commit/8329b73be8): Move "words of encouragement" from mini-buffer to Idris repl banner.
+ [71ab6a35e3](https://github.com/idris-hackers/idris-mode/commit/71ab6a35e3): Update semantic source highlighting file only in changed code parts reducing buffer "flickering".
+ [e5ef933366](https://github.com/idris-hackers/idris-mode/commit/e5ef933366): Only display Idris repl buffer on load without moving the point.
+ [9e931bf1ff](https://github.com/idris-hackers/idris-mode/commit/9e931bf1ff): Make `idris-list-holes-on-load` obsolete in favour of `idris-list-holes` command.
+ [446c67cec7](https://github.com/idris-hackers/idris-mode/commit/446c67cec7): Ensure Idris connection established and current file loaded  when running interactive command `idris-thing-at-point`
+ [cb71c82e13](https://github.com/idris-hackers/idris-mode/commit/cb71c82e13): Make commands `idris-pop-to-repl` and `idris-switch-to-output-buffer` obsolete in favour of `idris-switch-to-repl` command.
+ [7697b8b95e](https://github.com/idris-hackers/idris-mode/commit/7697b8b95e): Make `idris-print-definition-of-name` obsolete in favour of `idris-print-definition-of-name-at-point`.
+ [600c8f584b](https://github.com/idris-hackers/idris-mode/commit/600c8f584b): Make Idris info buffers derived from Help mode making handling them more align with general Emacs conventions.

### Bug fixes

+ Fix `idris-identifier-face` looking wrong in `org-mode` blocks and the like.
+ [3c3a87c66c](https://github.com/idris-hackers/idris-mode/commit/3c3a87c66c): Fix failure to find beginning of function type definition when lifting hole and function name contains underscore.
+ [62c3ad2b0d](https://github.com/idris-hackers/idris-mode/commit/62c3ad2b0d): Do not display unnecessary `*idris-process*` buffer when loading file.
+ [486be1b740](https://github.com/idris-hackers/idris-mode/commit/486be1b740): Improve `idris-case-dwim` to make case expression from hole in edge case point positions.
+ [8ff4a2d9d5](https://github.com/idris-hackers/idris-mode/commit/8ff4a2d9d5) [4f654a8b20ba6](https://github.com/idris-hackers/idris-mode/commit/4f654a8b20ba6) [c84ed5a733](https://github.com/idris-hackers/idris-mode/commit/c84ed5a733): Improve resetting state on `idris-quit` making it easier to switch Idris version or restart connection.
+ [1382948269](https://github.com/idris-hackers/idris-mode/commit/1382948269): Consider `-` as operator in idris-thing-at-point . Fixes https://github.com/idris-community/idris2-mode/issues/16
+ [216945f4a6](https://github.com/idris-hackers/idris-mode/commit/216945f4a6): Fix "off-by-one" source code highlighting in Idris 1.
+ [928f785bb7](https://github.com/idris-hackers/idris-mode/commit/928f785bb7): Allow loading multiple files with identical name but in different directories.
+ [ac029bc67e](https://github.com/idris-hackers/idris-mode/commit/ac029bc67e): Remove extra white-space included by Idris2 on `idris-add-clause` command.
+ [24ce417b69](https://github.com/idris-hackers/idris-mode/commit/24ce417b69): Preserve point position after adding warning overlay. Resolves part of: https://github.com/idris-community/idris2-mode/issues/36
+ [a47811be8b](https://github.com/idris-hackers/idris-mode/commit/a47811be8b): Remove `{{{{{ VAL }}}}}` value from `idris-name-key` text property fixing some command depending on it to have meaningful or no value.
+ [3e7cbb331f](https://github.com/idris-hackers/idris-mode/commit/3e7cbb331f): Improve compatibility with Idris2
+ [43b6036c99](https://github.com/idris-hackers/idris-mode/commit/43b6036c99): Display key binding for `idris-case-split` and `idris-make-cases-from-hole` in menu. Resolves: https://github.com/idris-hackers/idris-mode/issues/447

## 1.1

+ New customisation settings:
  + `idris-display-words-of-encouragement` toggles showing words of encouragement.
  + `idris-completion-via-compiler` toggles use of the Idris compiler to provide completion.
    + Tab in the repl still uses `completion-at-point`.
+ Improvements to testing harness, with support for testing against Idris2.
+ Migration of CI from Travis to GitHub Actions
  + Deprecation of older emacs for testing.
+ More support for IDE Protocol Version2 (i.e. Idris2).
+ Upstream changes as contributed to the Idris2-Mode on idris-community.
  + Improvements to Makefile
  + Changes to semantic faces

## 1.0

+ Idris mode has been brought uptodate with Idris1
+ Basic navigation commands added

### Fixes

+ Fix regular expression when matching on `.ipkg` extensions
+ Prevent completion-error when identifier is at beginning of buffer
+ Internal code changes
+ Better development testing with travis

### UX

+ `C-u C-c C-l` flags the buffer as dirty
+ Add images back into the repl
+ Disable the Idris event log by default
+ When Idris returns no proof search, do not delete the metavas
+ Remove references to idris-event-buffer-name when idris-log-events is nil
+ Fix idris-simple-indent-backtab
+ Give operator chars "." syntax and improve idris-thing-at-point
+ Conditional semantic faces for light/dark backgrounds

### Documentation

+ General fix ups
+ Document a way of reducing excessive frames


## 2016 Feb 29

 * It is possible to customize what happens to the focus of the current
   window when the type checking is performed and type errors are detected,
   now the user can choose between two options: 1) the current window stays
   focused or 2) the focus goes to the `*idris-notes*` buffer.
   The  true or false value of the variable
   `idris-stay-in-current-window-on-compiler-error` controls this behaviour.

## 0.9.19

 * The variable `idris-packages` has been renamed to
   `idris-load-packages`. If you have this as a file variable, please
   rename it.
 * The faces `idris-quasiquotation-face` and
   `idris-antiquotation-face` have been added, for compiler-supported
   highlighting of quotations. They are, by default, without
   properties, but they can be customized if desired.
 * Active terms can now be right-clicked. The old "show term widgets"
   command is no longer necessary, and will be removed in an upcoming
   release.
 * The case split command can be run on a hole, causing it to be filled
   with a prototype for a case split expression. Case-splitting a pattern
   variable has the same effect as before.
 * There is support for the interactive elaborator, which may replace
   the interactive prover in a future release. To use this, set
   `idris-enable-elab-prover` to non-`nil`.

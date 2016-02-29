# Changes

This file documents the user-interface changes in idris-mode, starting
with release 0.9.19.

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

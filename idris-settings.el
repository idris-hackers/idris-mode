;;; idris-settings.el --- Contains settings for idris-mode

;; Copyright (C) 2013 Hannes Mehnert and David Raymond Christiansen

;; Author: Hannes Mehnert <hannes@mehnert.org> and David Raymond Christiansen <david@davidchristiansen.dk>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'idris-keys)

;;;; Main settings

(defgroup idris nil "Idris mode" :prefix 'idris :group 'languages)

(defcustom idris-interpreter-path "idris"
  "The path to the Idris interpreter."
  :type 'file
  :group 'idris)

(defcustom idris-interpreter-flags '()
  "The command line arguments passed to the Idris interpreter."
  :type '(repeat string)
  :group 'idris)

(defcustom idris-warnings-printing (list 'warnings-tree)
  "How to print warnings: tree view ('warnings-tree) in REPL ('warnings-repl)."
  :group 'idris
  :type '(repeat symbol)
  :options '(warnings-tree warnings-repl))

(defcustom idris-pretty-printer-width 100
  "The default width to use for pretty-printing."
  :group 'idris
  :type '(choice (integer :tag "Columns")
                 (const :tag "Unlimited" nil)))


(defcustom idris-show-help-text t
  "Show explanatory text in idris-mode's auxiliary buffers if non-nil.
Advanced users may wish to disable this."
  :group 'idris
  :type 'boolean)

(defcustom idris-stay-in-current-window-on-compiler-error nil
  "Stay in current window if type checking fails."
  :group 'idris
  :type 'boolean)

(defcustom idris-semantic-source-highlighting t
  "Use the Idris compiler's semantic source information to highlight Idris code.
If `debug', log failed highlighting to buffer `*Messages*'."
  :group 'idris
  :type '(choice (boolean :tag "Enable")
                 (const :tag "Debug" debug)))

(defcustom idris-semantic-source-highlighting-max-buffer-size 32768 ;; (expt 2 15)
  "Disable semantic source code highlighting if buffer exceds alloted size.
This is to ensure that Emacs stays responsive for large Idris files."
  :group 'idris
  :type 'integer)

(defcustom idris-log-events nil
  "If non-nil, communications between Emacs and Idris are logged.

The log is placed in `idris-event-buffer-name'."
  :group 'idris
  :type 'boolean)

(defcustom idris-completion-via-compiler t
  "Use the compiler to make suggestions."
  :type 'boolean
  :group 'idris)

(defcustom idris-display-words-of-encouragement t
  "Send words of encouragement."
  :type 'boolean
  :group 'idris)

;;; Faces
(defface idris-active-term-face
  '((((background light))
     :background "lightgray")
    (((background dark))
     :background "darkgray"))
  "The face to highlight active terms."
  :group 'idris-faces)

(defface idris-semantic-type-face
  '((((background light))
      :foreground "blue")
    (((background dark))
      :foreground "cornflower blue"))
  "The face to be used to highlight types."
  :group 'idris-faces)

(defface idris-semantic-data-face
  '((((background light))
      :foreground "red")
    (((background dark))
      :foreground "firebrick1"))
  "The face to be used to highlight data and constructors."
  :group 'idris-faces)

(defface idris-semantic-function-face
  '((((background light))
      :foreground "darkgreen")
    (((background dark))
      :foreground "#A6E22E"))
  "The face to be used to highlight defined functions."
  :group 'idris-faces)

(defface idris-semantic-postulate-face
  '((t (:inherit idris-unsafe-face :weight semi-bold)))
  "The face to be used to highlight postulated values."
  :group 'idris-faces)

(defface idris-semantic-bound-face
  '((((background light))
     :foreground "purple")
    (((background dark))
     :foreground "MediumPurple1"))
  "The face to be used to highlight bound variables."
  :group 'idris-faces)

(defface idris-semantic-implicit-face
  '((t (:underline t)))
  "The face to be used to highlight implicit arguments."
  :group 'idris-faces)

(defface idris-semantic-namespace-face
  '((t (:italic t)))
  "The face to be used to highlight namespace declarations."
  :group 'idris-faces)

(defface idris-semantic-module-face
  '((t :inherit idris-semantic-namespace-face))
  "The face to be used to highlight namespace declarations."
  :group 'idris-faces)

(defface idris-quasiquotation-face nil
  "The face to be used to highlight quasiquotations in Idris source code."
  :group 'idris-faces)

(defface idris-antiquotation-face nil
  "The face to be used to highlight antiquotations in Idris source code."
  :group 'idris-faces)

(defface idris-loaded-region-face nil
  "The face to use for the currently-loaded region of a buffer.
Since semantic highlighting has been added, this face defaults to nothing,
but is provided for users who prefer the old behavior."
  :group 'idris-faces)

(defface idris-inline-doc-face
  '((t :inherit font-lock-doc-face))
  "The face shown for IdrisDoc while editing Idris files."
  :group 'idris-faces)

(defface idris-link-face
  '((t :inherit button))
  "The face shown for Web links in Idris documentation."
  :group 'idris-faces)

(defface idris-info-title-face
  '((t :inherit header-line))
  "Face for Idris headers and titles."
  :group 'idris-faces)

;;; Mode hooks
(defcustom idris-mode-hook '(turn-on-idris-simple-indent
                             turn-on-eldoc-mode)
  "Hook to run upon entering Idris mode.
You should choose at most one indentation style."
  :type 'hook
  :options '(turn-on-idris-simple-indent
             turn-on-eldoc-mode)
  :group 'idris)

(defcustom idris-mode-lidr-hook '()
  "Hook to run after opening a literate Idris file.
Use this to customize the display of non-code text."
  :type 'hook
  :group 'idris)

(defcustom idris-info-mode-hook ()
  "Hook to run when setting up Idris info buffers."
  :type 'hook
  :options ()
  :group 'idris)

(defcustom idris-repl-mode-hook ()
  "Hook to run when setting up the Idris REPL."
  :type 'hook
  :options ()
  :group 'idris)

(defcustom idris-compiler-notes-mode-hook ()
  "Hook to run when setting up the compiler notes buffers."
  :type 'hook
  :options ()
  :group 'idris)

(defgroup idris-hole-list nil
  "Options related to the Idris hole list buffer."
  :group 'idris)

(defcustom idris-hole-list-mode-hook ()
  "Hook to run when setting up the list of holes."
  :type 'hook
  :options ()
  :group 'idris-hole-list)

(defcustom idris-hole-show-on-load t
  "Show the current holes on successful load."
  :type 'boolean
  :group 'idris)

(defcustom idris-hole-list-show-expanded t
  "Show the hole list fully expanded by default.
This may be useful on wide monitors with lots of space for the hole buffer."
  :type 'boolean
  :group 'idris-hole-list)

(defcustom idris-enable-elab-prover nil
  "Whether or not to enable the interactive prover for elaborator reflection.
Disabled by default until Idris 0.9.19 because it requires a
change to ordinary prover interaction."
  :type 'boolean
  :group 'idris)

;;;; Other hooks

(autoload 'idris-set-current-pretty-print-width "idris-commands.el")
(defcustom idris-run-hook '(idris-set-current-pretty-print-width)
  "A hook to run when Idris is started."
  :type 'hook
  :group 'idris
  :options '(idris-set-current-pretty-print-width))

(defcustom idris-load-file-success-hook '(idris-list-holes
                                          idris-set-current-pretty-print-width)
  "Functions to call when loading a file is successful.
When `idris-hole-show-on-load' is set to nil the function `idris-list-holes'
will be removed from the list automatically and will not be executed."
  :type 'hook
  :options '(idris-list-holes
             idris-set-current-pretty-print-width)
  :group 'idris)

(defcustom idris-prover-success-hook '(idris-list-holes)
  "Functions to call when completing a proof.
When `idris-hole-show-on-load' is set to nil the function `idris-list-holes'
will be removed from the list automatically and will not be executed."
  :type 'hook
  :options '(idris-list-holes)
  :group 'idris-prover)

;;;; REPL settings

(defgroup idris-repl nil "Idris REPL" :prefix 'idris :group 'idris)

(defcustom idris-repl-banner-functions '(idris-repl-insert-logo
                                         idris-repl-animate-banner
                                         idris-repl-text-banner)
  "A list of functions that can attempt to insert a banner into the REPL.
If a function cannot insert a banner (for instance, if it is supposed
to insert a graphical banner but the current Emacs has no image support),
it returns nil. The functions in this list are run in order,
until one returns non-nil. Set to nil for no banner."
  :type 'hook
  :group 'idris-repl
  :options '(idris-repl-insert-logo
             idris-repl-animate-banner
             idris-repl-text-banner))

(defcustom idris-repl-show-idris-version t
  "Whether to show the Idris version on REPL startup."
  :type 'boolean
  :group 'idris-repl)

(defface idris-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for Idris output in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the Idris REPL."
  :group 'idris-repl)

(defface idris-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the Idris REPL."
  :group 'idris-repl)

(defcustom idris-repl-history-file nil
  "File to save the persistent REPL history to.

By default we assume Idris' default configuration home is:

  $HOME/.idris/idris-history.eld.

If you have installed/configured Idris differently, or are
using Idris2, then you may wish to customise this variable."

  :type 'string
  :group 'idris-repl)

(defcustom idris-repl-history-size 200
  "Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'idris-repl)

(defcustom idris-repl-history-file-coding-system
  'utf-8-unix
  "The coding system for the history file."
  :type 'symbol
  :group 'idris-repl)

(defcustom idris-repl-prompt-style 'short
  "What sort of prompt to show.
'long shows the Idris REPL prompt, while 'short shows a shorter one."
  :options '(short long)
  :type 'symbol
  :group 'idris-repl)

(defcustom idris-repl-show-repl-on-startup t
  "If non-nil, show the REPL window when Idris starts.
If nil, only do this when `idris-repl' was called interactively."
  :type 'boolean
  :group 'idris-repl)

(provide 'idris-settings)

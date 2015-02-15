;;; idris-settings.el --- Contains settings for idris-mode

;; Copyright (C) 2013 Hannes Mehnert and David Raymond Christiansen

;; Author: Hannes Mehnert <hannes@mehnert.org>

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
  "The path to the Idris interpreter"
  :type 'file
  :group 'idris)

(defcustom idris-interpreter-flags '()
  "The command line arguments passed to the Idris interpreter"
  :type '(repeat string)
  :group 'idris)

(defcustom idris-warnings-printing (list 'warnings-tree)
  "How to print warnings: tree view ('warnings-tree) in REPL ('warnings-repl)"
  :group 'idris
  :type '(repeat symbol)
  :options '(warnings-tree warnings-repl))

(defcustom idris-pretty-printer-width 100
  "The default width to use for pretty-printing."
  :group 'idris
  :type '(choice (integer :tag "Columns")
                 (const :tag "Unlimited" nil)))


(defcustom idris-show-help-text t
  "Show explanatory text in idris-mode's auxiliary buffers if
  non-nil. Advanced users may wish to disable this."
  :group 'idris
  :type 'boolean)

;;; Faces
(defface idris-active-term-face
  '((((background light))
     :background "lightgray")
    (((background dark))
     :background "darkgray"))
  "The face to highlight active terms"
  :group 'idris-faces)

(defface idris-semantic-type-face
  '((t (:foreground "blue")))
  "The face to be used to highlight types"
  :group 'idris-faces)

(defface idris-semantic-data-face
  '((t (:foreground "red")))
  "The face to be used to highlight data and constructors"
  :group 'idris-faces)

(defface idris-semantic-function-face
  '((t (:foreground "green")))
  "The face to be used to highlight defined functions"
  :group 'idris-faces)

(defface idris-semantic-bound-face
  '((t (:foreground "purple")))
  "The face to be used to highlight bound variables"
  :group 'idris-faces)

(defface idris-semantic-implicit-face
  '((t (:underline t)))
  "The face to be used to highlight implicit arguments"
  :group 'idris-faces)

(defface idris-loaded-region-face
  '((((background light) (min-colors 300)) (:background "#ccffcc"))
    (((background light) (min-colors 256)) (:background "pale green"))
    (((background dark) (min-colors 300))  (:background "#073642"))
    (((background dark) (min-colors 256))  (:background "DarkSlateGrey"))
    (t nil))
  "The face to use for the currently-loaded region of a buffer"
  :group 'idris-faces)

;;; Mode hooks
(defcustom idris-mode-hook '(turn-on-idris-simple-indent
                             idris-enable-clickable-imports
                             turn-on-eldoc-mode)
  "Hook to run upon entering Idris mode. You should choose at most one indentation style."
  :type 'hook
  :options '(turn-on-idris-simple-indent
             idris-enable-clickable-imports
             turn-on-eldoc-mode)
  :group 'idris)

(defcustom idris-mode-lidr-hook '()
  "Hook to run after opening a literate Idris file. Use this to customize the display of non-code text."
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

(defcustom idris-metavariable-list-mode-hook ()
  "Hook to run when setting up the list of metavariables."
  :type 'hook
  :options ()
  :group 'idris)

(defcustom idris-metavariable-show-on-load t
  "Show the current metavariables on successful load."
  :type 'boolean
  :group 'idris)

(defcustom idris-metavariable-list-show-expanded nil
  "Show the metavariable list fully expanded by default. This may be useful on wide monitors
with lots of space for the metavariable buffer."
  :type 'boolean
  :group 'idris)

;;;; Other hooks
(defcustom idris-run-hook '(idris-set-current-pretty-print-width)
  "A hook to run when Idris is started."
  :type 'hook
  :group 'idris
  :options '(idris-set-current-pretty-print-width))

;;;; REPL settings

(defgroup idris-repl nil "Idris REPL" :prefix 'idris :group 'idris)

(defcustom idris-repl-banner-functions '(idris-repl-insert-logo
                                         idris-repl-animate-banner
                                         idris-repl-text-banner)
  "A list of functions that can attempt to insert a banner into
the REPL. If a function cannot insert a banner (for instance, if
it is supposed to insert a graphical banner but the current Emacs
has no image support), it returns `nil'. The functions in this
list are run in order, until one returns non-`nil'.
Set to `nil' for no banner."
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

(defcustom idris-repl-history-file "~/.idris/idris-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'idris-repl)

(defcustom idris-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'idris-repl)

(defcustom idris-repl-history-file-coding-system
  'utf-8-unix
  "*The coding system for the history file."
  :type 'symbol
  :group 'idris-repl)

(defcustom idris-repl-prompt-style 'short
  "What sort of prompt to show. 'long shows the Idris REPL prompt, while 'short shows a shorter one."
  :options '(short long)
  :type 'symbol
  :group 'idris-repl)

(provide 'idris-settings)

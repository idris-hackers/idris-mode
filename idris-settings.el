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
  '((t (:slant italic)))
  "The face to be used to highlight implicit arguments"
  :group 'idris-faces)

(defcustom idris-mode-hook '(turn-on-idris-indentation)
  "Hook to run upon entering Idris mode."
  :type 'hook
  :options '(turn-on-idris-indentation)
  :group 'idris)

(defcustom idris-use-yasnippet-expansions t
  "Use yasnippet if available for completing interactive Idris commands"
  :type 'boolean
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

;;;; REPL settings

(defgroup idris-repl nil "Idris REPL" :prefix 'idris :group 'idris)

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

(provide 'idris-settings)

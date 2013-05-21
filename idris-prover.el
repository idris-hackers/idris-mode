;;; idris-prover.el --- Prover mode for Idris

;; Copyright (C) 2013 Hannes Mehnert

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

(defun idris-prover-event-hook-function (event)
  (destructure-case event
    ((:start-proof-mode name target)
     (idris-repl-write-string (concat "Start proof of " name))
     t)
    ((:end-proof-mode name target)
     (idris-repl-write-string (concat "End proof of " name))
     t)
    ((:write-goal goal target)
     (idris-repl-write-string goal)
     t)
    (t nil)))


(provide 'idris-prover)

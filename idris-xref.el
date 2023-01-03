;;; idris-xref.el --- Xref backend for Idris  -*- lexical-binding: t -*-
;; Copyright (C) 2022  Marek L.

;; Author: Marek L <nospam.keram@gmail.com>
;; Keywords: languages, Idris, Xref

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'xref)
(require 'inferior-idris)
(require 'idris-common-utils)

;; TODO: Define as idris-file-extension
(defconst idris-xref-idris-file-extension "idr")

(defgroup idris-xref nil "Idris Xref Backend." :prefix 'idris :group 'idris)

(defcustom idris-xref-idris-source-location ""
  "Path to local Idris language codebase repository."
  :type 'directory
  :group 'idris-xref)

(defcustom idris-xref-idris-source-locations '()
  "List of additional directories to perform lookup for a term.
To support jump to definition for Idris build-in types
set `idris-xref-idris-source-location' instead."
  :type '(repeat directory)
  :group 'idris-xref)

;; TODO: Memoize
(defun idris-xref-idris-source-directories ()
  "Return list of directories from Idris repository to do lookup for a term."
  (when (and idris-xref-idris-source-location (file-directory-p idris-xref-idris-source-location))
    (let ((src-dir (expand-file-name "src" idris-xref-idris-source-location))
          (libs-dir (expand-file-name "libs" idris-xref-idris-source-location)))
      (when (and (file-directory-p src-dir) (file-directory-p libs-dir))
        (cons src-dir
              (seq-filter #'file-directory-p
                          (mapcar (lambda (dir) (expand-file-name dir libs-dir))
                                  (seq-remove (lambda (dir) (string-match-p "\\." dir))
                                              (directory-files libs-dir)))))))))

(defun idris-xref-backend ()
  "An `xref-backend-functions' implementation for `idris-mode'."
  'idris)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql idris)))
  "Alias for `idris-name-at-point'."
  (idris-name-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql idris)) symbol)
  "Return a list of Xref objects candidates matching SYMBOL."

  (mapcar #'idris-xref-make-xref
          (mapcar #'idris-xref-normalise (idris-xref-find-definitions symbol))))

(cl-defmethod xref-backend-apropos ((_backend (eql idris)))
  "Not yet supported."
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql idris)))
  "Not yet supported."
  nil)

(defun idris-xref-find-definitions (symbol)
  "Return a list of Idris candidate locations matching SYMBOL."
  (car (idris-eval `(:name-at ,symbol))))

(defun idris-xref-normalise (candidate)
  "Return normalised CANDIDATE.

It will try add filename absolute path if not present and
update coordinates to be indexed from 1 as expected by Emacs."
  (pcase-let ((`(,term (:filename ,fn)
                       (:start ,start-line ,start-col)
                       (:end ,end-line ,end-col))
               candidate))
    (let ((new-fn (idris-xref-filepath term fn)))
      `(,term (:filename ,new-fn)
              (:start ,(1+ start-line) ,start-col)
              (:end ,(1+ end-line) ,end-col)))))

(defun idris-xref-make-xref (location)
  "Return a new Xref object from LOCATION."
  (pcase-let ((`(,term (:filename ,fn)
                       (:start ,start-line ,start-col)
                       (:end ,_end-line ,_end-col))
               location))
    (xref-make term
               (if (file-exists-p fn)
                   (xref-make-file-location fn start-line start-col)
                 (xref-make-bogus-location (format "%s : %s" term fn))))))

(defun idris-xref-filepath (term file)
  "Return filepath for TERM.
If FILE is path to existing file returns the FILE.
Otherwise try find the corresponding FILE for TERM in `idris-xref-directories'."
  (if (file-exists-p file)
      file
    (let ((file-paths (idris-xref-abs-filepaths term (idris-xref-directories))))
      (if (null file-paths)
          file
        ;; TODO: Instead of getting the first filepath build list of candidates
        (car file-paths)))))

(defun idris-xref-directories ()
  "List of directories to perform lookup for file containing a term.
The list consist of `idris-process-current-working-directory',
`idris-xref-idris-source-directories' and `idris-xref-idris-source-locations'."
  (append
    (and idris-process-current-working-directory (list idris-process-current-working-directory))
    (idris-xref-idris-source-directories)
    (seq-filter #'file-directory-p idris-xref-idris-source-locations)))

(defun idris-xref-relative-filepath-from-term (term)
  "Return relative Idris file path created from TERM."
  (let* ((term-parts (reverse (butlast (split-string term "\\."))))
         (file-base (pop term-parts))
         (file-name (concat file-base "." idris-xref-idris-file-extension)))
    (apply 'idris-file-name-concat (reverse (cons file-name term-parts)))))

(defun idris-xref-abs-filepaths (term locations)
  "Return absolute filepaths build from TERM and LOCATIONS."
  (let ((rel-path (idris-xref-relative-filepath-from-term term)))
    ;; TODO: Check also the content of file for presence of the term
    (seq-filter #'file-exists-p
                (mapcar (lambda (loc) (expand-file-name rel-path loc))
                        locations))))

(provide 'idris-xref)
;;; idris-xref.el ends here

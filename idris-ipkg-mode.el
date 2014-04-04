;;; idris-ipkg-mode.el --- Major mode for editing Idris package files -*- lexical-binding: t -*-

;; Copyright (C) 2014

;; Author: David Raymond Christiansen
;; URL: https://github.com/idris-hackers/idris-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24"))


;;; Commentary:

;; This is an Emacs mode for editing Idris packages. It requires the latest
;; version of Idris, and some features may rely on the latest Git version of
;; Idris.

;;; Code:

(require 'idris-core)
(require 'idris-settings)

(defface idris-ipkg-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris package keywords"
  :group 'idris-faces)

(defface idris-ipkg-package-name-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to highlight the name of the package"
  :group 'idris-faces)

(defconst idris-ipkg-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)
    st))

(defvar idris-ipkg-mode-keymap
  (make-sparse-keymap))

(defconst idris-ipkg-keywords
  '("package" "opts" "modules" "sourcedir" "makefile" "objs"))

(defconst idris-ipkg-font-lock-defaults
  `(,idris-ipkg-keywords))

;;;###autoload
(define-derived-mode idris-ipkg-mode prog-mode "Idris Pkg"
  "Major mode for Idris package files
     \\{idris-ipkg-mode-map}
Invokes `idris-ipkg-mode-hook'."
  :group 'idris
  :syntax-table idris-ipkg-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       idris-ipkg-font-lock-defaults))

(push '("\\.ipkg$" . idris-ipkg-mode) auto-mode-alist)

(provide 'idris-ipkg-mode)

;;; idris-ipkg-mode.el ends here

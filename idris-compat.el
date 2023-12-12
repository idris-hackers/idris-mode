;;; idris-compat.el --- compatibility functions for Emacs 24.1 -*- lexical-binding: t -*-

;;; Commentary:
;; This file defines defvar-local, which was introduced in Emacs 24.3, and string-suffix-p, from Emacs 24.4.

;;; Code:
(require 'subr-x nil 'no-error)   ; Additional utilities, Emacs 24.4 and upwards

(eval-and-compile
  (unless (featurep 'subr-x)
    ;; `subr-x' function for Emacs 24.3 and below
    (defsubst string-blank-p (string)
      "Check whether STRING is either empty or only whitespace."
      (string-match-p "\\`[ \t\n\r]*\\'" string))))

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

;;; The following function is copyright FSF, from GNU Emacs 24.4 source code.
(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string  &optional ignore-case)
    "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil
                                  string start-pos nil ignore-case))))))

;; gensym fun introduced at or before Emacs version 26.1.
(unless (fboundp 'gensym)
  (defalias 'gensym 'cl-gensym))

(if (fboundp 'file-name-concat)
    (defalias 'idris-file-name-concat 'file-name-concat)
  (defun idris-file-name-concat (&rest components)
    (let ((dirs (butlast components)))
      (concat (apply 'concat (mapcar 'file-name-as-directory dirs))
              (car (reverse components))))))

(if (fboundp 'string-limit)
    (defalias 'idris-string-limit 'string-limit)
  ;; Extracted from Emacs 28* 'subr-x
  (defun idris-string-limit (string length &optional end coding-system)
    "Return a substring of STRING that is (up to) LENGTH characters long.
If STRING is shorter than or equal to LENGTH characters, return the
entire string unchanged.

If STRING is longer than LENGTH characters, return a substring
consisting of the first LENGTH characters of STRING.  If END is
non-nil, return the last LENGTH characters instead.

If CODING-SYSTEM is non-nil, STRING will be encoded before
limiting, and LENGTH is interpreted as the number of bytes to
limit the string to.  The result will be a unibyte string that is
shorter than LENGTH, but will not contain \"partial\" characters,
even if CODING-SYSTEM encodes characters with several bytes per
character.

When shortening strings for display purposes,
`truncate-string-to-width' is almost always a better alternative
than this function."
    (unless (natnump length)
      (signal 'wrong-type-argument (list 'natnump length)))
    (if coding-system
        (let ((result nil)
              (result-length 0)
              (index (if end (1- (length string)) 0)))
          (while (let ((encoded (encode-coding-char
                                 (aref string index) coding-system)))
                   (and (<= (+ (length encoded) result-length) length)
                        (progn
                          (push encoded result)
                          (cl-incf result-length (length encoded))
                          (setq index (if end (1- index)
                                        (1+ index))))
                        (if end (> index -1)
                          (< index (length string)))))
            ;; No body.
            )
          (apply #'concat (if end result (nreverse result))))
      (cond
       ((<= (length string) length) string)
       (end (substring string (- (length string) length)))
       (t (substring string 0 length))))))

(provide 'idris-compat)
;;; idris-compat.el ends here

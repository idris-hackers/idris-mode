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

(if (fboundp 'file-name-parent-directory)
    (defalias 'idris-file-name-parent-directory 'file-name-parent-directory)
  ;; Extracted from Emacs 29+ https://github.com/emacs-mirror/emacs/blob/master/lisp/files.el
  (defun idris-file-name-parent-directory (filename)
    "Return the directory name of the parent directory of FILENAME.
If FILENAME is at the root of the filesystem, return nil.
If FILENAME is relative, it is interpreted to be relative
to `default-directory', and the result will also be relative."
    (let* ((expanded-filename (expand-file-name filename))
           (parent (file-name-directory (directory-file-name expanded-filename))))
      (cond
       ;; filename is at top-level, therefore no parent
       ((or (null parent)
            ;; `equal' is enough, we don't need to resolve symlinks here
            ;; with `file-equal-p', also for performance
            (equal parent expanded-filename))
        nil)
       ;; filename is relative, return relative parent
       ((not (file-name-absolute-p filename))
        (file-relative-name parent))
       (t
        parent)))))

(provide 'idris-compat)
;;; idris-compat.el ends here

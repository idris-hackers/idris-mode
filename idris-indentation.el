;;; idris-indentation.el -- indentation module for Idris Mode

;; Based on haskell-indentation.el, part of haskell-mode

;; Copyright 2009 Kristof Bastiaensen

;; Author: 2009 Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Installation:
;;
;; To turn indentation on for all Idris buffers under Idris mode
;; <http://www.haskell.org/haskell-mode/> add this to .emacs:
;;
;;    (add-hook idris-mode-hook 'turn-on-idris-indentation)
;;
;; Otherwise, call `idris-indentation-mode'.
;;

;;; Code:

(eval-when-compile (require 'cl)) ;needed for def of incf
(require 'syntax nil t)			; Emacs 21 add-on

;; temp
(defconst idris-literate nil)

(defgroup idris-indentation nil
  "Idris indentation."
  :group 'idris
  :prefix "idris-indentation-")

(defcustom idris-indentation-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'idris-indentation)

(defcustom idris-indentation-layout-offset 2
  "Extra indentation to add before expressions in a idris layout list."
  :type 'integer
  :group 'idris-indentation)

(defcustom idris-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'idris-indentation)

(defcustom idris-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'idris-indentation)

(defcustom  idris-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'idris-indentation)

(defcustom idris-indentation-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'idris-indentation)

(defcustom idris-indentation-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'idris-indentation)

(defcustom idris-indentation-birdtrack-extra-space t
  "Append a space after every birdtrack in literate mode."
  :type 'boolean
  :group 'idris-indentation)


;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

(defconst idris-indentation-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'idris-newline-and-indent)
    (define-key keymap [backspace] 'idris-indentation-delete-backward-char)
    (define-key keymap [?\C-d] 'idris-indentation-delete-char)
    keymap))

;;;###autoload
(define-minor-mode idris-indentation-mode
  "Idris indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " Ind"
  :keymap idris-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when idris-indentation-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function)
         'idris-indentation-indent-line)
    (set (make-local-variable 'normal-auto-fill-function)
         'idris-indentation-auto-fill-function)
    (set (make-local-variable 'idris-indent-last-position)
         nil)))

(defun turn-on-idris-indentation ()
  "Turn on the idris-indentation minor mode."
  (interactive)
  (idris-indentation-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defun idris-current-column ()
  "Compute current column according to idris syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun kill-indented-line (&optional arg)
  "`kill-line' for indented text.
Preserves indentation and removes extra whitespace"
  (interactive "P")
  (let ((col (idris-current-column))
	(old-point (point)))
    (cond ((or (and (numberp arg) (< arg 0))
	       (and (not (looking-at "[ \t]*$"))
		    (or (not (numberp arg)) (zerop arg))))
					;use default behavior when calling with a negative argument
					;or killing (once) from the middle of a line
	   (kill-line arg))
	  ((and (skip-chars-backward " \t") ;always true
		(bolp)
		(save-excursion
		  (forward-line arg)
		  (not (looking-at "[ \t]*$"))))
					; killing from an empty line:
					; preserve indentation of the next line
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (point)))
	   (skip-chars-forward " \t")
	   (if (> (idris-current-column) col)
	       (move-to-column col)))
	  (t				; killing from not empty line:
					; kill all indentation
	   (goto-char old-point)
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (skip-chars-forward " \t")
			  (point)))))))

(defun idris-indentation-auto-fill-function ()
  (when (> (idris-current-column) fill-column)
    (while (> (idris-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
	  (indent (car (last (idris-indentation-find-indentations)))))
      (newline)
      (when (eq idris-literate 'bird)
	(insert ">"))
      (indent-to indent)
      (end-of-line))))

(defun idris-indentation-reindent (col)
  (beginning-of-line)
  (delete-region (point)
		 (progn
		   (when (and (eq idris-literate 'bird)
			      (eq (char-after) ?>))
		     (forward-char))
		   (skip-syntax-forward "-")
		   (point)))
  (when (eq idris-literate 'bird)
    (insert ">"))
  (indent-to col))

(defun idris-indentation-current-indentation ()
  (if (eq idris-literate 'bird)
      (save-excursion
	(beginning-of-line)
	(forward-char)
	(skip-syntax-forward "-")
	(current-column))
    (current-indentation)))

(defun idris-indentation-outside-bird-line () 
  (and (eq idris-literate 'bird)
       (or (< (current-column) 2)
	   (save-excursion
	     (beginning-of-line)
	     (not (eq (char-after) ?>))))))

(defun idris-newline-and-indent ()
  (interactive)
  (if (idris-indentation-outside-bird-line)
      (progn
	(delete-horizontal-space)
	(newline))
    (on-parse-error
     (newline)
     (let* ((cc (idris-current-column))
            (ci (idris-indentation-current-indentation))
            (indentations (idris-indentation-find-indentations)))
       (skip-syntax-forward "-")
       (if (prog1 (and (eolp)
                       (not (= (idris-current-column) ci)))
	     (delete-horizontal-space)
	     (if (not (eq idris-literate 'bird))
		 (newline)
	       (when idris-indentation-birdtrack-extra-space
		 (indent-to 2))
	       (newline)
	       (insert "> ")))
           (idris-indentation-reindent
            (max (idris-indentation-butlast indentations)
                 (idris-indentation-matching-indentation
                  ci indentations)))
         (idris-indentation-reindent (idris-indentation-matching-indentation
                                        cc indentations)))))))

(defun idris-indentation-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
	   col)
	  ((null (cdr indentations))
	   (car indentations))
	  ((<= col (car last-pair))
	   col)
	  (t (car last-pair)))))

(defun idris-indentation-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun idris-indentation-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun idris-indentation-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
	 (while indentations
	   (if (or (null (cdr indentations))
		   (<= col (cadr indentations)))
	       (throw 'return (car indentations))
	     (setq indentations (cdr indentations))))
	 col)))

(defun idris-indentation-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun idris-indentation-indent-line ()
  (when (save-excursion
	  (beginning-of-line)
	  (not (nth 8 (syntax-ppss))))
    (let ((ci (idris-indentation-current-indentation))
          (start-column (idris-current-column)))
      (cond ((> (idris-current-column) ci)
	     (save-excursion
	       (move-to-column ci)
	       (idris-indentation-reindent
		(idris-indentation-one-indentation
		 ci (idris-indentation-find-indentations)))))

	    ((= (idris-current-column) ci)
	     (idris-indentation-reindent
	      (idris-indentation-next-indentation
	       ci (idris-indentation-find-indentations))))

	    (t (move-to-column ci)
	       (idris-indentation-reindent
		(idris-indentation-matching-indentation
		 ci (idris-indentation-find-indentations)))))
      (cond ((not (= (idris-current-column) start-column))
             (setq idris-indent-last-position nil))
            ((not idris-indentation-cycle-warn)
             (idris-indentation-reindent
              (idris-indentation-next-indentation
               -1
               (idris-indentation-find-indentations))))
            ((not (equal (point) idris-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq idris-indent-last-position (point)))
            (t
             (idris-indentation-reindent
              (idris-indentation-next-indentation
               -1
               (idris-indentation-find-indentations))))))))

(defun idris-indentation-delete-backward-char (n)
  (interactive "p")
  (on-parse-error
   (delete-backward-char n)
   (cond
    ((idris-indentation-outside-bird-line)
     (delete-backward-char n))
    ((and delete-selection-mode
	  mark-active
	  (not (= (point) (mark))))
     (delete-region (mark) (point)))
    ((or (= (idris-current-column) 0)
	 (> (idris-current-column) (idris-indentation-current-indentation))
	(nth 8 (syntax-ppss)))
     (delete-backward-char n))
    (t (let* ((ci (idris-indentation-current-indentation))
	      (pi (idris-indentation-previous-indentation
		   ci (idris-indentation-find-indentations))))
	 (save-excursion
	   (cond (pi
		  (move-to-column pi)
		  (delete-region (point)
				 (progn (move-to-column ci)
					(point))))
		 (t
		  (beginning-of-line)
		  (delete-region (max (point-min) (- (point) 1))
				 (progn (move-to-column ci)
					(point)))))))))))

(defun idris-indentation-delete-char (n)
  (interactive "p")
  (if (idris-indentation-outside-bird-line)
      (delete-char n)
    (on-parse-error (delete-char n)
     (cond
      ((and delete-selection-mode
	    mark-active
	    (not (= (point) (mark))))
       (delete-region (mark) (point)))
      ((and (eq idris-literate 'bird)
	    (looking-at "\n> "))
       (delete-char (+ n 2)))
      ((or (eolp)
	   (>= (idris-current-column) (idris-indentation-current-indentation))
	   (nth 8 (syntax-ppss)))
       (delete-char n))
      (t
       (let* ((ci (idris-indentation-current-indentation))
	      (pi (idris-indentation-previous-indentation
		   ci (idris-indentation-find-indentations))))
	 (save-excursion
	   (if (and pi (> pi (idris-current-column)))
	       (move-to-column pi))
	   (delete-region (point)
			  (progn (move-to-column ci)
				 (point))))))))))

(defun idris-indentation-goto-least-indentation ()
  (beginning-of-line)
  (if (eq idris-literate 'bird)
      (catch 'return
	(while t
	  (when (not (eq (char-after) ?>))
	    (forward-line)
	    (forward-char 2)
	    (throw 'return nil))
	  (let ((ps (nth 8 (syntax-ppss))))
	    (when ps ;; inside comment or string
	      (goto-char ps)
	      (beginning-of-line)))
	  (when (and (>= 2 (idris-indentation-current-indentation))
		     (not (looking-at ">\\s-*$")))
	    (forward-char 2)
	    (throw 'return nil))
	  (when (bobp)
	    (forward-char 2)
	    (throw 'return nil))
	  (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
	(forward-comment (- (buffer-size)))
	(beginning-of-line)
	(let ((ps (nth 8 (syntax-ppss))))
	  (when ps ;; inside comment or string
	    (goto-char ps)))
	(when (= 0 (idris-indentation-current-indentation))
	  (throw 'return nil))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)

(defun idris-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
	  (layout-indent 0)
	  (parse-line-number 0)
	  (current-indent idris-indentation-layout-offset)
	  (starter-indent idris-indentation-layout-offset)
	  (left-indent idris-indentation-layout-offset)
	  (case-fold-search nil)
	  current-token
	  following-token
	  possible-indentations)
      (idris-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
	  (idris-indentation-first-indentation)
	(setq current-token (idris-indentation-peek-token))
	(catch 'parse-end
	  (idris-indentation-toplevel)
	  (when (not (equal current-token 'end-tokens))
	    (parse-error "Illegal token: %s" current-token)))
	possible-indentations))))

(defun idris-indentation-first-indentation ()
  (if (eq idris-literate 'bird) '(2) '(0)))

(defun idris-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (idris-indentation-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
	    (and (skip-syntax-forward "-")
		 (eolp)
		 (not (> (forward-line 1) 0))
		 (not (nth 4 (syntax-ppss)))))
	  (idris-indentation-parse-to-indentations)
	(idris-indentation-first-indentation)))
     (t
      (idris-indentation-parse-to-indentations)))))

(defconst idris-indentation-toplevel-list
  '(("module" . idris-indentation-module)
    ("data" . idris-indentation-data)
    ("type" . idris-indentation-data)
    ("newtype" . idris-indentation-data)
    ("class" . idris-indentation-class-declaration)
    ("instance" . idris-indentation-class-declaration )))

(defconst idris-indentation-type-list
  '((":"    . (lambda () (idris-indentation-statement-right #'idris-indentation-type)))
    ("("     . (lambda () (idris-indentation-list #'idris-indentation-type
						    ")" "," nil)))
    ("["     . (lambda () (idris-indentation-list #'idris-indentation-type
						    "]" "," nil)))
    ("{"     . (lambda () (idris-indentation-list #'idris-indentation-type
						    "}" "," nil)))))

(defconst idris-indentation-expression-list
  '(("data" . idris-indentation-data)
    ("type" . idris-indentation-data)
    ("newtype" . idris-indentation-data)
    ("if"    . (lambda () (idris-indentation-phrase
			   '(idris-indentation-expression
			     "then" idris-indentation-expression
			     "else" idris-indentation-expression))))
    ("let"   . (lambda () (idris-indentation-phrase
			   '(idris-indentation-declaration-layout
			     "in" idris-indentation-expression))))
    ("do"    . (lambda () (idris-indentation-with-starter
			   #'idris-indentation-expression-layout nil)))
    ("mdo"   . (lambda () (idris-indentation-with-starter
			   #'idris-indentation-expression-layout nil)))
    ("rec"   . (lambda () (idris-indentation-with-starter
			   #'idris-indentation-expression-layout nil)))
    ("case"  . (lambda () (idris-indentation-phrase
			   '(idris-indentation-expression
			     "of" idris-indentation-case-layout))))
    ("\\"    . (lambda () (idris-indentation-phrase
			   '(idris-indentation-expression
			     "->" idris-indentation-expression))))
    ("proc"  . (lambda () (idris-indentation-phrase
			   '(idris-indentation-expression
			     "->" idris-indentation-expression))))
    ("where" . (lambda () (idris-indentation-with-starter
			   #'idris-indentation-declaration-layout nil t)))
    (":"    . (lambda () (idris-indentation-statement-right #'idris-indentation-type)))
    ("="     . (lambda () (idris-indentation-statement-right #'idris-indentation-expression)))
    ("<-"    . (lambda () (idris-indentation-statement-right #'idris-indentation-expression)))
    ("("     . (lambda () (idris-indentation-list #'idris-indentation-expression
						    ")" '(list "," "->") nil)))
    ("["     . (lambda () (idris-indentation-list #'idris-indentation-expression
						    "]" "," "|")))
    ("{"     . (lambda () (idris-indentation-list #'idris-indentation-expression
						    "}" "," nil)))))
	  
(defun idris-indentation-expression-layout ()
  (idris-indentation-layout #'idris-indentation-expression))

(defun idris-indentation-declaration-layout ()
  (idris-indentation-layout #'idris-indentation-declaration))

(defun idris-indentation-case-layout ()
  (idris-indentation-layout #'idris-indentation-case))

(defun idris-indentation-fundep ()
  (idris-indentation-with-starter
   (lambda () (idris-indentation-separated
	       #'idris-indentation-fundep1 "," nil))
   nil))

(defun idris-indentation-fundep1 ()
  (let ((current-indent (idris-current-column)))
    (while (member current-token '(value "->"))
      (idris-indentation-read-next-token))
    (when (and (equal current-token 'end-tokens)
	       (member following-token '(value "->")))
      (idris-indentation-add-indentation current-indent))))

(defun idris-indentation-toplevel ()
  (idris-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token idris-indentation-toplevel-list)))
       (if parser
	   (funcall (cdr parser))
	 (idris-indentation-declaration))))))

(defun idris-indentation-type ()
  (let ((current-indent (idris-current-column)))
    (catch 'return
      (while t
	(cond
	 ((member current-token '(value operator "->"))
	  (idris-indentation-read-next-token))
	 
	 ((equal current-token 'end-tokens)
	  (when (member following-token
			'(value operator no-following-token
				"->" "(" "[" "{" ":"))
	    (idris-indentation-add-indentation current-indent))
	  (throw 'return nil))
	 
	 (t (let ((parser (assoc current-token idris-indentation-type-list)))
	      (if (not parser)
		  (throw 'return nil)
		(funcall (cdr parser))))))))))

(defun idris-indentation-data ()
  (idris-indentation-with-starter
   (lambda ()
     (when (equal current-token "instance")
       (idris-indentation-read-next-token))
     (idris-indentation-type)
     (cond ((equal current-token "=")
	    (idris-indentation-with-starter
	     (lambda () (idris-indentation-separated #'idris-indentation-type "|" "deriving"))
	     nil))
	   ((equal current-token "where")
	    (idris-indentation-with-starter
	     #'idris-indentation-expression-layout nil))))
   nil))

(defun idris-indentation-class-declaration ()
  (idris-indentation-with-starter
   (lambda ()
     (idris-indentation-type)
     (when (equal current-token "|")
       (idris-indentation-fundep))
     (when (equal current-token "where")
       (idris-indentation-with-starter
	#'idris-indentation-expression-layout nil)))
   nil))

(defun idris-indentation-module ()
  (idris-indentation-with-starter
   (lambda ()
     (let ((current-indent (idris-current-column)))
       (idris-indentation-read-next-token)
       (when (equal current-token 'end-tokens)
         (idris-indentation-layout #'idris-indentation-toplevel)))) nil))

(defun idris-indentation-list (parser end sep stmt-sep)
  (idris-indentation-with-starter
   `(lambda () (idris-indentation-separated #',parser
					      ,sep
					      ,stmt-sep))
   end))

(defun idris-indentation-with-starter (parser end &optional where-expr?)
  (let ((starter-column (idris-current-column))
	(current-indent current-indent)
	(left-indent (if (= (idris-current-column) (idris-indentation-current-indentation))
			 (idris-current-column) left-indent)))
    (idris-indentation-read-next-token)
    (when (equal current-token 'end-tokens)
      (if (equal following-token end)
	  (idris-indentation-add-indentation starter-column)
        (if where-expr?
            (idris-indentation-add-where-post-indent left-indent)
	  (idris-indentation-add-indentation
	   (+ left-indent idris-indentation-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (idris-current-column))
	   (starter-indent (min starter-column current-indent))
	   (left-indent (if end (+ current-indent idris-indentation-starter-offset)
			  left-indent)))
      (funcall parser)
      (cond ((equal current-token 'end-tokens)
	     (when (equal following-token end)
	       (idris-indentation-add-indentation starter-indent))
	     (when end (throw 'parse-end nil))) ;; add no indentations
	    ((equal current-token end)
	     (idris-indentation-read-next-token)) ;; continue
	    (end (parse-error "Illegal token: %s" current-token))))))

(defun idris-indentation-case ()
  (idris-indentation-expression)
  (cond ((equal current-token 'end-tokens)
	 (idris-indentation-add-indentation current-indent))
	((equal current-token "|")
	 (idris-indentation-with-starter
	  (lambda () (idris-indentation-separated #'idris-indentation-case "|" nil))
	  nil))
	((equal current-token "->")
	 (idris-indentation-statement-right #'idris-indentation-expression))
	;; otherwise fallthrough
	))

(defun idris-indentation-statement-right (parser)
    (idris-indentation-read-next-token)
    (when (equal current-token 'end-tokens)
      (idris-indentation-add-indentation
       (+ left-indent idris-indentation-left-offset))
      (throw 'parse-end nil))
    (let ((current-indent (idris-current-column)))
	  (funcall parser)))

(defun idris-indentation-simple-declaration ()
  (idris-indentation-expression)
  (cond ((equal current-token "=")
	 (idris-indentation-statement-right #'idris-indentation-expression))
	((equal current-token ":")
	 (idris-indentation-statement-right #'idris-indentation-type))
	((and (equal current-token 'end-tokens)
	      (equal following-token "="))
	 (idris-indentation-add-indentation current-indent)
	 (throw 'parse-end nil))))

(defun idris-indentation-declaration ()
  (idris-indentation-expression)
  (cond ((equal current-token "|")
	 (idris-indentation-with-starter
	  (lambda () (idris-indentation-separated #'idris-indentation-expression "," "|"))
	  nil))
	((equal current-token 'end-tokens)
	 (when (member following-token '("|" "=" ":" ","))
	   (idris-indentation-add-indentation current-indent)
	   (throw 'parse-end nil)))))

(defun idris-indentation-layout (parser)
  (if (equal current-token "{")
      (idris-indentation-list parser "}" ";" nil)
    (idris-indentation-implicit-layout-list parser)))

(defun idris-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" ":"
		  value operator no-following-token)))

(defun idris-indentation-expression ()
  (let ((current-indent (idris-current-column)))
    (catch 'return
      (while t
	(cond
	 ((member current-token '(value operator))
	  (idris-indentation-read-next-token))

	 ((equal current-token 'end-tokens)
	  (cond ((equal following-token "where")
		 (idris-indentation-add-where-pre-indent))
		((idris-indentation-expression-token following-token)
		 (idris-indentation-add-indentation
		  current-indent)))
	  (throw 'return nil))

	 (t (let ((parser (assoc current-token idris-indentation-expression-list)))
	      (when (null parser)
		(throw 'return nil))
	      (funcall (cdr parser))
	      (when (and (equal current-token 'end-tokens)
			 (equal (car parser) "let")
			 (= idris-indentation-layout-offset current-indent)
			 (idris-indentation-expression-token following-token))
		;; inside a layout, after a let construct
		(idris-indentation-add-layout-indent)
		(throw 'parse-end nil))
	      (unless (member (car parser) '("(" "[" "{" "do" "case"))
		(throw 'return nil)))))))))

(defun idris-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (idris-indentation-find-indentations)))
	(str "")
	(pos 0))
    (while indentations
      (when (>= (car indentations) pos)
	(setq str (concat str (make-string (- (car indentations) pos) ?\ )
			  "|"))
	(setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun idris-indentation-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
	     (idris-indentation-at-separator))

	    ((equal current-token stmt-separator)
	     (setq starter-indent (idris-current-column))
	     (idris-indentation-at-separator))

	    ((equal current-token 'end-tokens)
	     (cond ((or (equal following-token separator)
			(equal following-token stmt-separator))
		    (idris-indentation-add-indentation starter-indent)
		    (throw 'parse-end nil)))
	     (throw 'return nil))

	    (t (throw 'return nil))))))

(defun idris-indentation-at-separator ()
  (let ((separator-column
	 (and (= (idris-current-column) (idris-indentation-current-indentation))
	      (idris-current-column))))
    (idris-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
	   (idris-indentation-add-indentation current-indent)
	   (throw 'return nil))
	  (separator-column ;; on the beginning of the line
	   (setq current-indent (idris-current-column))
	   (setq starter-indent separator-column)))))

(defun idris-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (idris-current-column))
	 (current-indent (idris-current-column))
	 (left-indent (idris-current-column)))
    (catch 'return
      (while t
	(let ((left-indent left-indent))
	  (funcall parser))
	(cond ((member current-token '(layout-next ";"))
	       (idris-indentation-read-next-token))
	      ((equal current-token 'end-tokens)
	       (when (or (idris-indentation-expression-token following-token)
					 (equal following-token ";"))
			 (idris-indentation-add-layout-indent))
	       (throw 'return nil))
	      (t (throw 'return nil))))))
  ;; put idris-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (idris-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun idris-indentation-phrase (phrase)
  (idris-indentation-with-starter
   `(lambda () (idris-indentation-phrase-rest ',phrase))
   nil))

(defun idris-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (idris-current-column)))
      (funcall (car phrase)))
    (cond
     ((equal current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
	    ((equal following-token (cadr phrase))
	     (idris-indentation-add-indentation starter-indent)
	     (throw 'parse-end nil))
	    ((equal (cadr phrase) "in")
	     (when (= left-indent layout-indent)
	       (idris-indentation-add-layout-indent)
	       (throw 'parse-end nil)))
	    (t (throw 'parse-end nil))))

     ((null (cdr phrase)))
     
     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (idris-current-column) (idris-indentation-current-indentation)))
	     (lines-between (- parse-line-number starter-line))
	     (left-indent (if (<= lines-between 0)
			      left-indent
			    starter-indent)))
	(idris-indentation-read-next-token)
	(when (equal current-token 'end-tokens)
	  (idris-indentation-add-indentation
	   (cond ((member (cadr phrase) '("then" "else"))
		  (+ starter-indent idris-indentation-ifte-offset))
		 ((member (cadr phrase) '("in" "->"))
		  ;; expression ending in another expression
		  (if on-new-line
		      (+ left-indent idris-indentation-starter-offset)
		    left-indent))
		 (t (+ left-indent idris-indentation-left-offset))))
	  (throw 'parse-end nil))
	(idris-indentation-phrase-rest (cddr phrase))))

     ((equal (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun idris-indentation-add-indentation (indent)
  (idris-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent idris-indentation-layout-offset)
     indent)))

(defun idris-indentation-add-layout-indent ()
  (idris-indentation-push-indentation layout-indent))

(defun idris-indentation-add-where-pre-indent ()
  (idris-indentation-push-indentation
   (+ layout-indent idris-indentation-where-pre-offset))
  (if (= layout-indent idris-indentation-layout-offset)
      (idris-indentation-push-indentation
       idris-indentation-where-pre-offset)))

(defun idris-indentation-add-where-post-indent (indent)
  (idris-indentation-push-indentation
   (+ indent idris-indentation-where-post-offset)))

(defun idris-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
	    (< indent (car possible-indentations)))
    (setq possible-indentations
	  (cons indent possible-indentations))))

(defun idris-indentation-token-test ()
  (let ((current-token nil)
	(following-token nil)
	(layout-indent 0)
	(parse-line-number 0)
	(indentation-point (mark)))
    (idris-indentation-read-next-token)))

(defun idris-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
	 'end-tokens)
	((eq current-token 'layout-end)
	 (cond ((> layout-indent (idris-current-column))
		'layout-end)
	       ((= layout-indent (idris-current-column))
		(setq current-token 'layout-next))
	       ((< layout-indent (idris-current-column))
		(setq current-token (idris-indentation-peek-token)))))
	((eq current-token 'layout-next)
	 (setq current-token (idris-indentation-peek-token)))
	((> layout-indent (idris-current-column))
	 (setq current-token 'layout-end))
	(t
	 (idris-indentation-skip-token)
	 (if (>= (point) indentation-point)
	     (progn
	       (setq following-token
		     (if (= (point) indentation-point)
			 (idris-indentation-peek-token)
		       'no-following-token))
	       (setq current-token 'end-tokens))
	   (when (= (idris-current-column) (idris-indentation-current-indentation))
	     ;; on a new line
	     (setq current-indent (idris-current-column))
	     (setq left-indent (idris-current-column))
	     (setq parse-line-number (+ parse-line-number 1)))
	   (cond ((> layout-indent (idris-current-column))
		  (setq current-token 'layout-end))
		 ((= layout-indent (idris-current-column))
		  (setq current-token 'layout-next))
		 (t (setq current-token (idris-indentation-peek-token))))))))

(defun idris-indentation-peek-token ()
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
	 (match-string 1))
	((looking-at "[][(){}[,;]")
	 (match-string 0))
	((looking-at "\\(\\\\\\|->\\|<-\\|:\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
	 (match-string 1))
	((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
	 'operator)
	(t 'value)))

(defun idris-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))
    
    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at	; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
    ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))
    (while (and (eq idris-literate 'bird)
		(bolp)
		(eq (char-after) ?>))
      (forward-char)
      (forward-comment (buffer-size)))))

(provide 'idris-indentation)
;;; idris-indentation.el ends here

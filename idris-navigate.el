;;; idris-navigate.el --- navigate in Idris code     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; Commentary: 

;; Provide a reliable jump to start and end of a top-level form - and some more
;; subroutines.

;; Key-setting working here:
;; (define-key idris-mode-map [(control meta a)] 'idris-backward-toplevel)
;; (define-key idris-mode-map [(control meta e)] 'idris-forward-toplevel)

;; (define-key idris-mode-map [(meta p)] 'idris-backward-statement) ;; current indent upwards
;; (define-key idris-mode-map [(meta n)] 'idris-forward-statement) ;; travels EOL downwards

;; Tested with
;; GNU Emacs 24.4.1 (i586-pc-linux-gnu, GTK+ Version 3.14.5)
;;  of 2017-09-12 on x86-csail-01, modified by Debian
;; 
;;    passed  1/4  idris-backard-statement-navigation-test-2pTac9
;;    passed  2/4  idris-backard-toplevel-navigation-test-2pTac9
;;    passed  3/4  idris-forward-statement-navigation-test-2pTac9
;;    passed  4/4  idris-forward-toplevel-navigation-test-2pTac9

;; Ran 4 tests, 4 results as expected (2020-08-21 20:10:42+0200)

;; Tests probably should go into another file

;;; Code:

(defvar idris-debug-p nil
  "Switch to test-buffer when t")

;; (setq idris-debug-p t)

(defvar idris-verbose-p nil)

(defvar idris-max-specpdl-size max-specpdl-size
  "Protect against eternal loop.")

(defvar idris-literal-delim-re "\""
  "When looking at beginning of string.")

(defvar idris-expression-skip-chars "^ (:=#\t\r\n\f"
  "idris-expression assumes chars indicated possible composing a idris-expression, skip it.")
(make-variable-buffer-local 'idris-expression-skip-chars)

(defvar idris-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.")

(defvar idris-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar idris-expression-re "[^ =#\t\r\n\f]+"
  "Ar-expression assumes chars indicated possible composing a idris-expression, when ‘looking-at’ or -back.")
(make-variable-buffer-local 'idris-expression-re)

(defvar idris-delimiter-regexp "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs.")
(make-variable-buffer-local 'idris-delimiter-regexp)

(defvar idris-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "idris-expression assumes chars indicated probably will not compose a idris-expression. ")

(defvar idris-not-expression-chars " #\t\r\n\f"
  "idris-expression assumes chars indicated probably will not compose a idris-expression. ")

(defvar idris-partial-expression-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
  "idris-partial-expression assumes chars indicated possible composing a idris-partial-expression, skip it. ")
;; (setq idris-partial-expression-backward-chars "^] .=,\"'()[{}:#\t\r\n\f")

(defvar idris-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")
;; (setq idris-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")

(defvar idris-partial-expression-re (concat "[" idris-partial-expression-backward-chars (substring idris-partial-expression-forward-chars 1) "]+"))
;; (setq idris-partial-expression-re (concat "[" idris-partial-expression-backward-chars "]+"))

(defvar idris-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "idris-expression assumes chars indicated possible composing a idris-expression, skip it.")
(make-variable-buffer-local 'idris-expression-skip-regexp)

(defmacro idris--escaped-p ()
  "Return t if char is preceded by an odd number of backslashes."
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro idris--preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (idris--escaped-p))))

(defun idris-fix-comment-start ()
  "Comment at point might not have a padding."
  (if (and comment-start (string-match "[ \t]$" comment-start))
      (concat comment-start "*")
    comment-start))

(defun idris--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of expression after a semicolon.

Returns position reached if point was moved.
Optional argument LIMIT limit."
  (let ((orig (point)))
    (and (< 0 (abs
               (skip-chars-backward "^;" (or limit (line-beginning-position)))))
         (skip-chars-forward " \t" (line-end-position))
         (and (< (point) orig) (point)))))

(defmacro idris--current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (idris--escaped-p))))

;;; string-strip stuff ends here
(defcustom empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :group 'convenience)
(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'idris-empty-line-p))

(defun idris-empty-line-p (&optional iact)
  "Return t if cursor is at an empty line, nil otherwise.
Optional argument IACT saying interactively called."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at empty-line-p-chars)))
    (looking-at empty-line-p-chars)))

(defun idris-in-comment-p (&optional start)
  "Return the beginning of current line's comment, if inside.
Optional argument START start."
  (interactive)
  (let* ((pps (parse-partial-sexp (or start (point-min)) (point)))
	 (erg (and (nth 4 pps) (nth 8 pps))))
    (unless (or erg (nth 3 pps))
      (when (or (eq (car (syntax-after (point))) 11)
		(ignore-errors (looking-at comment-start)))
	(setq erg (point))))
  erg))

(defun idris-backward-comment (&optional pos)
  "Got to beginning of a commented section.
Optional argument POS start."
  (interactive)
  (let ((erg pos)
	last)
    (when erg (goto-char erg))
    (while (and (not (bobp)) (setq erg (idris-in-comment-p)))
      (when (< erg (point))
	(goto-char erg)
	(setq last (point)))
      (skip-chars-backward " \t\r\n\f"))
    (when last (goto-char last))
    last))

(defun idris-forward-comment (&optional pos char)
  "Go to end of (next) commented section following point.

Optional args position and ‘comment-start’ character
Travel empty lines
Optional argument POS orig.
Optional argument CHAR comment start."
  (interactive)
  (let ((orig (or pos (point)))
	(char (or char (string-to-char comment-start)))
	last)
    (unless (idris-in-comment-p)
      (search-forward comment-start nil t 1))
    (while (and (not (eobp))
		(forward-comment 99999)))
    (when (eq (point) orig)
      ;; forward-comment fails sometimes
      (while
	  (and (not (eobp)) (or (idris-in-comment-p)(eq (point) orig)))
	(setq last (line-end-position))
	(forward-line 1)
	(end-of-line)
	;; (setq orig (point))
	))
    (and (eq orig (point)) (prog1 (forward-line 1) (back-to-indentation))
	 (while (member (char-after) (list char 10))(forward-line 1)(back-to-indentation)))
    ;; go
    (when last
      (goto-char last)
      (skip-chars-forward " \t\r\n\f")
      (back-to-indentation))
    (unless (eq (point) orig)
      (point))))

;; Navigate
(defun idris-skip-blanks-and-comments (&optional arg pps orig)
  "Go forward over empty lines and comments alike.

Stop at first non-empty char.
With negative arg go backward. "
  (interactive)
  (let ((arg (or arg 1))
	(pos (point))
	(orig (or orig (point)))
	(pps (or pps (parse-partial-sexp (point-min) (point)))))
    (if (< 0 arg)
        (progn
          (skip-chars-forward " \t\r\n")
          (when (or (and pps (nth 4 pps))(idris-in-comment-p))
	    (end-of-line)
	    (skip-chars-forward " \t\r\n\f"))
          (when (empty-line-p)
            (forward-line arg))
          (when (> (point) pos)
            (idris-skip-blanks-and-comments arg nil orig))
	  (< orig (point)))
      (skip-chars-backward " \t\r\n")
      (when (or (and pps (nth 4 pps))(idris-in-comment-p))
        (goto-char (or (and pps (nth 4 pps))(nth 8 pps))))
      (> orig (point)))))

(defun idris-in-string-p ()
  "Return start position, if inside or at opening delimiter.

Otherwise return nil."
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
	 (erg (and (nth 3 pps) (nth 8 pps)))
	 (la (unless (or erg (eobp))
	       (and (eq (char-syntax (char-after)) 34)
		    ;; look for closing char
		    (save-excursion
		      (forward-char 1)
		      (nth 3 (parse-partial-sexp (point-min) (point))))
		    (point)))))
    (when (interactive-p) (message "%s" (or erg la)))
    (or erg la)))

;; Expression
(defun idris-backward-expression ()
  "Go to the beginning of a compound expression.

A a compound expression might be concatenated,
thus composed by minor expressions.

If already at the beginning or before a expression,
go to next expression in buffer upwards"
  (interactive)
  (let (erg)
    (setq erg (idris--beginning-of-expression-intern))
    (when (and idris-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun idris--beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ( ;; (empty-line-p)
        (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
             (eq 9 (char-after))(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (idris--beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward idris-expression-skip-chars))
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back idris-assignment-regexp (line-beginning-position)))
              ((looking-back idris-operator-regexp (line-beginning-position))
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (idris--beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (idris--beginning-of-expression-intern orig)))
       ;; concatenated strings
       ((looking-back (concat idris-string-delim-re idris-expression-re idris-string-delim-re idris-operator-regexp idris-string-delim-re idris-expression-re idris-string-delim-re) (line-beginning-position))
        (goto-char (match-beginning 0))
        (while (looking-back (concat idris-string-delim-re idris-expression-re idris-string-delim-re idris-operator-regexp) (line-beginning-position) t)
          (goto-char (match-beginning 0)))
        (skip-chars-backward idris-expression-skip-chars))
       ;; before comment
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (idris--beginning-of-expression-intern orig)))
       ((and (< (point) orig)(looking-at (concat idris-expression-re idris-delimiter-regexp))))
       ((looking-back (concat "[^ \t\n\r\f]+" idris-delimiter-regexp) (line-beginning-position))
        (goto-char (match-beginning 0))
	(skip-chars-backward idris-expression-skip-chars)
        (unless (or (looking-back idris-assignment-regexp (line-beginning-position)) (looking-back "^[ \t]*" (line-beginning-position)))
          (idris--beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back idris-assignment-regexp (line-beginning-position))
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (idris--beginning-of-expression-intern orig))
       ((looking-back idris-operator-regexp (line-beginning-position))
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward idris-expression-skip-chars)))
          (idris--beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'" (line-beginning-position))
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back idris-assignment-regexp (line-beginning-position))
          (idris--beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[" (line-beginning-position))
        (forward-char -1)
        (unless (looking-back idris-assignment-regexp (line-beginning-position))
          (idris--beginning-of-expression-intern orig)))
       ((looking-back "[\])}]" (line-beginning-position))
        (forward-char -1)
        (unless (looking-back idris-assignment-regexp (line-beginning-position))
          (idris--beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back idris-expression-re (line-beginning-position))
        (skip-chars-backward idris-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*" (line-beginning-position)) (looking-back idris-assignment-regexp (line-beginning-position)))
          (idris--beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" idris-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*" (line-beginning-position))
          (idris--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]" (line-beginning-position)))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (idris--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back idris-expression-re (line-beginning-position)))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward idris-expression-skip-chars)))
          (idris--beginning-of-expression-intern orig)))
       ((and (looking-at idris-expression-re) (not (looking-back "[ \t\r\n\f]" (line-beginning-position))))
        (unless (< 0 (abs (skip-chars-backward idris-expression-skip-chars)))
          (idris--beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*=" (line-beginning-position)))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (idris--beginning-of-expression-intern orig)))
      (unless (or (eq (point) orig)(looking-at "[ \t]*#"))
        (setq erg (point)))
      erg)))

(defun idris-forward-expression (&optional orig done repeat pps)
  "Go to the end of a compound expression.

Operators are ignored.
Optional argument ORIG Position.
Optional argument DONE status.
Optional argument REPEAT counter.
Optional argument PPS result of ‘parse-partial-sexp’."
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((comment-start (idris-fix-comment-start))
	  (repeat (or (and repeat (1+ repeat)) 0))
	  (pps (or pps (parse-partial-sexp (point-min) (point))))
          (orig (or orig (point)))
          erg)
      (if (< idris-max-specpdl-size repeat)
	  (error "`idris-forward-expression' reached loops max")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
	  (idris-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and comment-start (looking-at (concat "[ \t]*" comment-start))(looking-back "^[ \t]*" (line-beginning-position)))
	  (while (and (looking-at (concat "[ \t]*" comment-start)) (not (eobp)))
	    (forward-line 1))
	  (idris-forward-expression orig done repeat))
	 ;; inside string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (idris-forward-expression orig done repeat))
	 ((looking-at "\"\"\"\\|'''\\|\"\\|'")
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (idris-forward-expression orig done repeat))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (idris-forward-expression orig done repeat))
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (idris-forward-expression orig done repeat))
	 ((and (eq orig (point)) (looking-at idris-operator-regexp))
	  (goto-char (match-end 0))
	  (idris-forward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (skip-chars-forward idris-expression-skip-chars)))
	  (setq done t)
	  (idris-forward-expression orig done repeat))
	 ;; at colon following arglist
	 ((looking-at ":[ \t]*$")
	  (forward-char 1)))
	(unless (or (eq (point) orig)(and (eobp)(bolp)))
	  (setq erg (point)))
	(when (and idris-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun idris-down-expression ()
  "Go to the beginning of next expression downwards in buffer.

Return position if expression found, nil otherwise."
  (interactive)
  (let* ((orig (point))
         (erg
          (cond ((idris--end-of-expression-p)
                 (and (idris-forward-expression) (idris-backward-expression)))
                ((ignore-errors (< orig (progn (idris-forward-expression) (idris-backward-expression))))
                 (point))
                (t (goto-char orig) (and (idris-forward-expression) (idris-forward-expression)(idris-backward-expression))))))
    (when (and idris-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; (defun idris--end-of-expression-intern (&optional orig)
;;   (unless (eobp)
;;     (let* ((orig (or orig (point)))
;;            (pps (syntax-ppss))
;;            erg
;;            ;; use by scan-lists
;;            parse-sexp-ignore-comments)
;;       (cond
;;        ((nth 1 pps)
;;         (goto-char (nth 1 pps))
;;         (let ((parse-sexp-ignore-comments t))
;;           (forward-list))
;;         (unless (or (looking-at "[ \t]*$")(looking-at idris-assignment-regexp))
;;           (idris--end-of-expression-intern orig)))
;;        ;; in comment
;;        ((nth 4 pps)
;;         (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
;;         (idris--end-of-expression-intern orig))
;;        ( ;; (empty-line-p)
;; 	(eq 9 (char-after))
;;         (while
;;             (and  ;; (empty-line-p)
;; 	     (eq 9 (char-after))(not (eobp)))
;;           (forward-line 1))
;;         (idris--end-of-expression-intern orig))
;;        ((looking-at (concat idris-string-delim-re idris-expression-re idris-string-delim-re idris-operator-regexp idris-string-delim-re idris-expression-re idris-string-delim-re))
;;         (goto-char (match-end 0))
;;         (while (looking-at (concat idris-operator-regexp idris-string-delim-re idris-expression-re idris-string-delim-re))
;;           (goto-char (match-end 0))))
;;        ;; inside string
;;        ((idris-in-string-p)
;;         (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
;;           (goto-char (match-end 0)))
;;         (while
;;             (nth 3 (syntax-ppss))
;;           (forward-char 1))
;;         (unless (looking-at "[ \t]*$")
;;           (idris--end-of-expression-intern orig)))
;;        ((looking-at "[(\[]")
;;         (forward-list)
;;         (unless (looking-at "[ \t]*$")
;;           (idris--end-of-expression-intern orig)))
;;        ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
;;         (while (and (looking-at "[ \t]*#") (not (eobp)))
;;           (forward-line 1))
;;         (idris--end-of-expression-intern orig))
;;        ((and (eq orig (point)) (looking-at idris-assignment-regexp))
;;         (goto-char (match-end 0))
;;         (if (looking-at "[(\[]")
;;             (forward-list 1)
;;           (idris--end-of-expression-intern orig)))
;;        ((looking-at (concat "[^ \t\n\r\f]*" idris-delimiter-regexp))
;;         (goto-char (match-end 0))
;;         (while (looking-at (concat "[^ \t\n\r\f]*" idris-delimiter-regexp))
;;           (goto-char (match-end 0)))
;;         (forward-char -1)
;;         (unless (looking-at (concat idris-assignment-regexp "\\|[ \t]*$\\|" idris-delimiter-regexp))
;;           (idris--end-of-expression-intern orig)))
;;        ((looking-at (concat "\\([[:alnum:] ]+ \\)" idris-assignment-regexp))
;; 	(goto-char (match-end 1))
;; 	(skip-chars-backward " \t\r\n\f"))
;;        ((and (eq orig (point)) (looking-at (concat "[ \t]*" "[^(\t\n\r\f]+" idris-operator-regexp)))
;; 	(skip-chars-forward " \t\r\n\f")
;; 	(when (< 0 (skip-chars-forward idris-expression-skip-chars))
;; 	  (idris--end-of-expression-intern orig)))
;;        ((and (eq orig (point)) (looking-at idris-not-expression-regexp))
;;         (skip-chars-forward idris-not-expression-chars)
;;         (unless (or (looking-at "[ \t]*$")(looking-at idris-assignment-regexp))
;;           (idris--end-of-expression-intern orig)))
;;        ((looking-at idris-expression-skip-regexp)
;;         (skip-chars-forward idris-expression-skip-chars)
;;         (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at idris-assignment-regexp))
;;           (idris--end-of-expression-intern orig)))
;;        ((and (eq (point) orig)
;; 	     (skip-chars-forward " \t\r\n\f")
;; 	     (< 0 (skip-chars-forward idris-expression-skip-chars)))
;; 	(idris--end-of-expression-intern orig)))

;;       (unless (or (eq (point) orig)(and (eobp)(bolp)))
;;         (setq erg (point)))
;;       erg)))

(defun idris-backward-partial-expression (&optional orig)
  "Go to the beginning of a partial expression.
Optional argument ORIG Position."
  (interactive)
  (let (erg)
    (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))(not (bobp))(forward-char -1))
    (when (idris-in-comment-p)
      (idris-backward-comment)
      (skip-chars-backward " \t\r\n\f"))
    ;; part of idris-partial-expression-forward-chars
    (when (member (char-after) (list ?\ ?\" ?' ?\) ?} ?\] ?: ?#))
      (forward-char -1))
    (skip-chars-backward idris-partial-expression-forward-chars)
    (when (< 0 (abs (skip-chars-backward idris-partial-expression-backward-chars)))
      (while (and (not (bobp)) (idris-in-comment-p)(< 0 (abs (skip-chars-backward idris-partial-expression-backward-chars))))))
    (when (< (point) orig)
      (unless
	  (and (bobp) (member (char-after) (list ?\ ?\t ?\r ?\n ?\f)))
	(setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun idris-forward-of-partial-expression ()
  "Go to the end of a partial expression.
Optional argument ORIG Position."
  (interactive)
  (let (erg)
    (skip-chars-forward idris-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun idris--beginning-of-expression-p (orig pps)
  "Return position, if cursor is at the beginning of a `expression'.

Return nil otherwise.
Argument ORIG Position.
Argument PPS result of ‘parse-partial-sexp’."
  (let (erg)
    (or (and pps (setq erg (eq 0 (nth 0 pps))))
	(save-excursion
	  (unless (and (eolp)(bolp))
	    (idris-forward-statement)
	    (idris-backward-statement))
	  (when (eq orig (point))
	    (setq erg orig))
	  erg))))

(defun idris--end-of-expression-p ()
  "Return position, if cursor is at the end of a expression, nil otherwise."
  (let ((orig (point))
	erg)
    (save-excursion
      (idris-backward-statement)
      (idris-forward-statement)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defvar toplevel-nostart-chars (list ?-))

(defun idris-backward-toplevel (&optional arg)
  "Go to the beginning of a toplevel form.

Returns position if successful, nil otherwise
Optional argument ARG times"
  (interactive "p")
  (unless (bobp)
    ;; (forward-line -1)
    ;; (beginning-of-line)
    (let* ((arg (or arg 1))
	   (orig (point))
	   (pps (parse-partial-sexp (point-min) (point)))
	   ;; set ppss start point
	   (limit (or (nth 8 pps) (point-min)))
	   (comment-start (idris-fix-comment-start))
	   erg this)
      ;; (unless (bobp)
      (while (and
	      (prog1 (re-search-backward "^[^ \t\n\f\r]" nil 'move arg)
		(beginning-of-line))
	      (or (ignore-errors (looking-at comment-start))(ignore-errors (looking-at comment-start-skip))
		  (and (setq this (save-excursion (ignore-errors (nth 8 (parse-partial-sexp limit (point))))))
		       (setq limit this))
		  (member (char-after) toplevel-nostart-chars)))
	(forward-line -1)
	(beginning-of-line))
      (when (< (point) orig)
	(setq erg (point))
	(when (interactive-p) (message "%s" erg)))
      erg)))

(defun idris--forward-toplevel-intern (orig pps)
  (let (last)
    (unless (idris--beginning-of-expression-p orig pps)
      (idris-backward-expression))
    (unless (eq 0 (current-column))
      (idris-backward-toplevel))
    (unless (< orig (point))
      (while (and
	      (not (eobp))
	      (save-excursion
		(idris-forward-expression orig nil nil pps)
		(setq last (point)))
	      (idris-down-expression)(< 0 (current-indentation)))))
    (and last (goto-char last))
    ))

(defun idris-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position.
Optional argument BEGINNING-OF-STRING-POSITION Position."
  (interactive)
  ;; (when idris-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when idris-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "idris-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and idris-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun idris-forward-toplevel (&optional arg)
  "Go to end of a toplevel form.

Returns position if successful, nil otherwise
Optional argument ARG times."
  (interactive "p")
  (unless (eobp)
    (let* ((arg (or arg 1))
	   (orig (point))
	   (pps (parse-partial-sexp (point-min) (point)))
	   ;; set ppss start point
	   (limit (or (nth 8 pps) (point-min)))
	   (comment-start (idris-fix-comment-start))
	   erg this)
      (idris-skip-blanks-and-comments)
      (while
	  (and
	   (progn (end-of-line)
		  (setq erg (re-search-forward "^[^ \t\n\f\r]" nil 'move arg)))
	   (or
	    (progn
	      (beginning-of-line)
	      (nth 8 (parse-partial-sexp (point-min) (point))))
	    (ignore-errors (when
			       (looking-at comment-start)
			     (forward-line 1)
			     t))
	    (ignore-errors (when (looking-at comment-start-skip)
			     (forward-line 1)
			     t))
	    (and (setq this (ignore-errors (nth 8 (parse-partial-sexp limit (point)))))
		 (setq limit this)))))
      (when erg
	(beginning-of-line)
	(skip-chars-backward " \t\r\n\f")
	(forward-line 1) (beginning-of-line))
      (when (< orig (point))
	(setq erg (point))
	(when (and idris-verbose-p (interactive-p)) (message "%s" erg)))
      erg)))

(defun idris-forward-toplevel-bol ()
  "Go to beginning of line after end of a toplevel form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (when (idris--forward-toplevel-intern orig (parse-partial-sexp (point-min) (point)))
	(if (eobp)
	    (newline 1)
	  (forward-line 1)
	  (beginning-of-line)))
      (when (< orig (point))
	(setq erg (point))))
    (when (and idris-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun idris-backward-statement (&optional orig done limit)
  "Go to the initial line of a simple expression.
Optional argument ORIG Position.
Optional argument DONE status.
Optional argument LIMIT limit."
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
        (unless done
          (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
               (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (idris-backward-statement orig done limit))
         ((nth 8 pps)
          ;; inside string
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (idris-backward-statement orig done limit))
         ((idris--preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (idris-backward-statement orig done limit))
         ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp))
                      (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (idris-backward-statement orig done limit)))
         ;; at inline comment
         ((looking-at "[ \t]*#")
          (when (idris--skip-to-semicolon-backward
                 (save-excursion (back-to-indentation)(point)))
            (skip-chars-forward " \t")
            (unless (bobp)
              (idris-backward-statement orig done limit))))
         ;; at beginning of string
         ((and (not done) (looking-at idris-literal-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (idris-backward-statement orig done limit))
         ;; after end of expression
         ;; ((and (not done) (eq (char-before) ?\;))
         ;;  (skip-chars-backward ";")
         ;;  (idris-backward-statement orig done limit))
         ;; at current indent
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (idris-backward-statement orig done limit))
         ((and (member (char-after) (list ?\" ?\'))
               (progn (back-to-indentation) (eq ?@ (char-after))))
          (back-to-indentation)
	  (when (< (point) orig) (setq done t))
          (idris-backward-statement orig done limit))
	 ((eq orig (point))
	  (back-to-indentation)
	  (when (< (point) orig)(setq done t))
	  (idris-backward-statement orig done limit))
	 )
        ;; return nil when before comment
	(unless (eq (current-indentation)  (current-column))
	  (back-to-indentation)
	  (setq done t)
	  (idris-backward-statement orig done limit))
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (when (< (point) orig)(setq erg (point))))
        (when (and idris-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defun idris-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

Optional argument REPEAT, the number of loops DONE already,
is checked for ‘idris-max-specpdl-size’ error.
Avoid eternal loops due to missing string delimters etc.
Optional argument ORIG Position."
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          pps err)
      (setq pps (parse-partial-sexp (point-min) (point)))
      (cond
       ((< idris-max-specpdl-size repeat)
        (error "Ar-forward-statement reached loops max.
If no error, customize `idris-max-specpdl-size'"))
       ;; string
       ((or (nth 3 pps)(eq (char-syntax (char-after)) 34))
        (when (idris-end-of-string)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (setq pps (parse-partial-sexp (point-min) (point)))
          (unless (and done
                       (not (or (nth 1 pps) (nth 8 pps)))
                       (eolp))
            (idris-forward-statement orig done repeat))))
       ;; in comment
       ((or (nth 4 pps)(eq (char-syntax (char-after)) ?<))
	(idris-forward-comment)
        (idris-forward-statement orig done repeat))
       ((idris--current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (idris--escaped-p))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (idris-forward-statement orig done repeat)))
       ((eq orig (point))
        (or (and
	     (< 0 (abs (skip-chars-forward (concat " \t\r\n\f'\"" comment-start))))
	     (eolp) (setq done t))
	    (end-of-line)
	    (skip-chars-backward " \t\r\n\f$"))
        (idris-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(end-of-line)
	(skip-chars-backward " \t\r\n\f")
	(setq done t)
	(idris-forward-statement orig done repeat))
       ;; list
       ((nth 1 pps)
	(unless done
	  (goto-char (nth 1 pps))
	  (ignore-errors (forward-sexp))
	  (setq done t)
	  (idris-forward-statement orig done repeat))))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9)))
	(setq erg (point)))
      (if (and idris-verbose-p err)
	  (message "%s" err)
	(and idris-verbose-p (interactive-p) (message "%s" erg)))
      erg)))

(defmacro idris-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `idris-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; requires idris.el
     ;; (and (featurep 'semantic) (unload-feature 'semantic))
     ;; (and (featurep 'idris) (unload-feature 'idris))
     (let (hs-minor-mode)
       (insert ,contents)
       (idris-mode)
       (goto-char (point-min))
       ;; (message "(current-buffer): %s" (current-buffer))
       (when idris-debug-p (switch-to-buffer (current-buffer))
	     ;; (font-lock-fontify-buffer)
	     (font-lock-ensure)
	     )
       ,@body)
     (sit-for 0.1)))

(defmacro idris-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `idris-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (and (featurep 'idris) (unload-feature 'idris))
     (let (hs-minor-mode)
       (insert ,contents)
       (idris-mode)
       (when idris-debug-p (switch-to-buffer (current-buffer))
	     ;; (font-lock-fontify-buffer)
	     (font-lock-ensure)
	     )
       ;; (message "ERT %s" (point))
       ,@body)
     (sit-for 0.1)))

(ert-deftest idris-backard-toplevel-navigation-test-2pTac9 ()
  "Test idris-backard-toplevel navigation command."
  (idris-test-with-temp-buffer
    "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var \[add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () \[remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String \[store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult \[store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () \[store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () \[failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () \[st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
    (goto-char (point-max))
    (idris-backward-toplevel)
    (should (looking-at "getData2 st"))
    ;; (goto-char (point-max))
    (search-backward "Number")
    (idris-backward-toplevel)
    (should (looking-at "getData failcount"))
    (search-backward "LoggedIn")
    (idris-backward-toplevel)
    (should (looking-at "interface DataStore"))
    ))

(ert-deftest idris-forward-toplevel-navigation-test-2pTac9 ()
  "Test idris-forard-toplevel navigation command."
  (idris-test-with-temp-buffer-point-min
    "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var \[add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () \[remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String \[store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult \[store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () \[store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () \[failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () \[st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
    (search-forward "DataStore")
    (idris-forward-toplevel)
    (should (empty-line-p))
    (skip-chars-backward " \t\r\n\f")
    (should (looking-back "Store LoggedOut]" (line-beginning-position)))
    (idris-forward-toplevel)
    (should (looking-at "getData failcount"))
    (idris-forward-toplevel)
    (should (empty-line-p))
    (skip-chars-backward " \t\r\n\f")
    (should (looking-back "getData failcount" (line-beginning-position)))
    ;; (goto-char (point-max))
    (search-forward "Number")
    (idris-forward-toplevel)
    (should (looking-back "getData2 st failcount" (line-beginning-position)))
    ))

(ert-deftest idris-backard-statement-navigation-test-2pTac9 ()
  "Test idris-backard-statement navigation command."
  (idris-test-with-temp-buffer
      "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var \[add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () \[remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String \[store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult \[store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () \[store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () \[failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () \[st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
    (goto-char (point-max))
    (idris-backward-statement)
    (should (looking-at "getData2 st"))
    (search-backward "Number")
    (idris-backward-statement)
    (should (looking-at "putStrLn ("))
    (idris-backward-statement)
    (should (looking-at "write failcount"))
    (search-backward "BadPassword")
    (idris-backward-statement)
    (should (looking-at "| BadPassword"))
    (idris-backward-statement)
    (should (looking-at "= do OK"))
    (idris-backward-statement)
    (should (looking-at "getData2 st"))
    (idris-backward-statement)
    (should (looking-at "ST m ()"))
    ))

(ert-deftest idris-forward-statement-navigation-test-2pTac9 ()
  "Test idris-forard-statement navigation command."
  (idris-test-with-temp-buffer-point-min
    "interface DataStore (m : Type -> Type) where
  data Store : Access -> Type

  connect : ST m Var \[add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () \[remove store (Store LoggedOut)]

  readSecret : (store : Var) -> ST m String \[store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult \[store ::: Store LoggedOut :->
                             (\\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () \[store ::: Store LoggedIn :-> Store LoggedOut]

getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var) -> ST m () \[failcount ::: State Integer]
getData failcount
   = do st <- call connect
        OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               disconnect st
                               getData failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        disconnect st
        getData failcount

getData2 : (ConsoleIO m, DataStore m) =>
           (st, failcount : Var) ->
           ST m () \[st ::: Store {m} LoggedOut, failcount ::: State Integer]
getData2 st failcount
   = do OK <- login st
           | BadPassword => do putStrLn \"Failure\"
                               fc <- read failcount
                               write failcount (fc + 1)
                               putStrLn (\"Number of failures: \" ++ show (fc + 1))
                               getData2 st failcount
        secret <- readSecret st
        putStrLn (\"Secret is: \" ++ show secret)
        logout st
        getData2 st failcount"
    (search-forward "DataStore")
    (idris-forward-statement)
    (should (looking-back "where" (line-beginning-position)))
    (idris-forward-statement)
    (should (looking-back "Access -> Type" (line-beginning-position)))
    (idris-forward-statement)
    (should (looking-back "Store LoggedOut)]" (line-beginning-position)))
    ))

(provide 'idris-navigate)
;;; idris-navigate.el ends here

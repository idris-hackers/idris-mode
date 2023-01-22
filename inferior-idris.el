;;; inferior-idris.el --- Run an Idris interpreter using S-Expression communication protocol -*- lexical-binding: t -*-

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

;;; Commentary:
;; Handle connection to Idris and expose `idris-eval' and `idris-eval-sync'
;; functions to be used by other modules for communication with Idris.
;;

(require 'idris-core)
(require 'idris-settings)
(require 'idris-common-utils)
(require 'cl-lib)
(require 'idris-events)
(require 'idris-log)
(require 'idris-warnings)

;;; Code:

(defvar idris-process nil
  "The Idris process.")

(defvar idris-connection nil
  "The Idris connection.")

(defvar idris-process-buffer-name (idris-buffer-name :process)
  "The name of the Idris process buffer.")

(defvar idris-connection-buffer-name (idris-buffer-name :connection)
  "The name of the Idris connection buffer.")

(defun idris-version-hook-function (event)
  (pcase event
    (`(:protocol-version ,version ,minor)
     (setf idris-protocol-version version)
     (setf idris-protocol-version-minor minor)
     (remove-hook 'idris-event-hooks 'idris-version-hook-function)
     t)))

(defvar-local idris-load-packages nil
  "The list of packages to be loaded by Idris.
Set using file or directory variables.")

(defun idris-compute-flags ()
  "Calculate the command line options to use when running Idris."
  (append (cl-loop for p in idris-load-packages
                   collecting "-p"
                   collecting p)
          idris-interpreter-flags
          (cl-mapcan #'funcall
                     idris-command-line-option-functions)))

(defvar idris-current-flags nil
  "The list of `command-line-args' actually passed to Idris.
This is maintained to restart Idris when the arguments change.")

(defun idris-connect (port)
  "Establish a connection with a Idris REPL at PORT."
  (when (not idris-connection)
    (setq idris-connection
          (open-network-stream "Idris IDE support" idris-connection-buffer-name "127.0.0.1" port))
    (add-hook 'idris-event-hooks 'idris-version-hook-function)
    (add-hook 'idris-event-hooks 'idris-log-hook-function)
    (add-hook 'idris-event-hooks 'idris-warning-event-hook-function)
    (add-hook 'idris-event-hooks 'idris-prover-event-hook-function)

    (if idris-hole-show-on-load
        (progn
          (add-hook 'idris-load-file-success-hook 'idris-list-holes)
          (add-hook 'idris-prover-success-hook 'idris-list-holes))
      (remove-hook 'idris-load-file-success-hook 'idris-list-holes-on-load)
      (remove-hook 'idris-load-file-success-hook 'idris-list-holes)
      ;; TODO: In future decouple prover sucess hook from being affected by
      ;; idris-hole-show-on-load variable
      (remove-hook 'idris-prover-success-hook 'idris-list-holes-on-load)
      (remove-hook 'idris-prover-success-hook 'idris-list-holes))

    (set-process-filter idris-connection 'idris-output-filter)
    (set-process-sentinel idris-connection 'idris-sentinel)
    (set-process-query-on-exit-flag idris-connection t)
    (setq idris-process-current-working-directory "")
    (run-hooks 'idris-run-hook)
    (message "Connection to Idris established.")))

(defun idris-sentinel (_process msg)
  (message "Idris disconnected: %s" (substring msg 0 -1))
  (when idris-connection
    (delete-process idris-connection)
    (setq idris-connection nil))
  (when idris-process
    (delete-process idris-process)
    (setq idris-process nil)))

(defvar idris-process-port-output-regexp (rx (? (group (+ any (not num)))) (group (+ (any num))))
  "Regexp used to match the port of an Idris process.")
(defvar idris-process-exact-port-output-regexp (rx bol (group (+ (any num))) eol)
  "Regexp to match port number.")
(defvar idris-exact-port-matcher 1
  "Port number matcher.")

(defvar idris-process-port-with-warning-output-regexp
  (rx (? (group (+ any (not num)))) (group (** 3 4 (any num)))))
;;      ^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^
;;           ^                          ^
;;           |                          |
;;           |                          +---- port number
;;           +------------------------------- warning message
(defvar idris-warning-matcher 1
  "Warning from Idris.")

(defvar idris-warning-port-matcher 2
  "Port number matcher with warning.")

;; idris-process-filter is broken in theoreticaly.
(defun idris-process-filter (string)
  "Accept STRING output from the process."
  (if idris-connection
      string
    ;; Idris sometimes prints a warning prior to the port number, which causes
    ;; `string-match' to return 0
    (cl-flet ((idris-warn (msg)
                          (unless (or (null msg) (string-blank-p msg))
                            (message "Warning from Idris: %s" msg))))
        (if (string-match idris-process-exact-port-output-regexp string)
            (idris-connect (string-to-number (match-string idris-exact-port-matcher string)))
          (if (not (string-match idris-process-port-with-warning-output-regexp string))
              (idris-warn string)
            (idris-warn (match-string idris-warning-matcher string))
            (idris-connect (string-to-number (match-string idris-warning-port-matcher string)))))
        "")))

(defun idris-show-process-buffer (string)
  "Show the Idris process buffer if STRING is non-empty."
  (when (> (length string) 0)
    (pop-to-buffer (get-buffer idris-process-buffer-name))))

(defun idris-output-filter (process string)
  "Accept STRING output from the socket and PROCESS all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (idris-connection-available-input process))

(defun idris-connection-available-input (process)
  "Process all complete messages which arrived from Idris PROCESS."
  (while (idris-have-input-p process)
    (let ((event (idris-receive process)))
      (idris-event-log event nil)
      (idris-dispatch-event event process))))

(defun idris-have-input-p (process)
  "Return `true' if a complete message is available in PROCESS buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (and (>= (buffer-size) 6)
         (>= (- (buffer-size) 6) (idris-decode-length)))))

(defun idris-receive (process)
  "Read a message from the Idris PROCESS."
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (let* ((length (idris-decode-length))
           (start (+ 6 (point)))
           (end (+ start length)))
      (cl-assert (cl-plusp length))
      (prog1 (save-restriction
               (narrow-to-region start end)
               (read (current-buffer)))
        (delete-region (point-min) end)))))

(defun idris-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun idris-send (sexp proc)
  "Send a SEXP to Idris over the PROC. This is the lowest level of communication."
  (let* ((msg (concat (idris-prin1-to-string sexp) "\n"))
         (string (concat (idris-encode-length (length msg)) msg)))
    (idris-event-log sexp t)
    (process-send-string proc string)))

(defun idris-encode-length (n)
  "Encode an N (integer) into a 24-bit hex string."
  (format "%06x" n))

(defun idris-prin1-to-string (sexp)
  "Like `prin1-to-string', but don't octal-escape non-ascii characters."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defvar idris-rex-continuations '()
  "List of (ID FUNCTION [FUNCTION]) continuations waiting for RPC results.
The first function will be called with a final result, and the second
 (if present) will be called with intermediate output results.")

(defvar idris-continuation-counter 1
  "Continuation serial number counter.")

(defvar idris-event-hooks '())

(defun idris-dispatch-event (event process)
  (or (run-hook-with-args-until-success 'idris-event-hooks event)
      (destructure-case event
        ((:emacs-rex form continuation &optional output-continuation)
         (let ((id (cl-incf idris-continuation-counter)))
           (idris-send `(,form ,id) process)
           (push (if output-continuation
                     (list id continuation output-continuation)
                   (list id continuation))
                 idris-rex-continuations)))
        ((:output value id)
         (let ((rec (assq id idris-rex-continuations)))
           ;; Commands that don't ask for :output don't get it
           (when (and rec (nth 2 rec))
             (funcall (nth 2 rec) value))))
        ((:return value id)
         (let ((rec (assq id idris-rex-continuations)))
           (cond (rec (setf idris-rex-continuations
                            (remove rec idris-rex-continuations))
                      (funcall (cadr rec) value))
                 (t (error "Unexpected reply: %S %S" id value))))))))

(cl-defmacro idris-rex ((&rest saved-vars) sexp intermediate &rest continuations)
  "Remote Execute SEXP.

\\(idris-rex (VAR ...) (SEXP) INTERMEDIATE CONTINUATIONS ...)

SAVED-VARS are a list of saved variables visible in the other forms.
Each VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the `princ'-ed version is sent to Idris.

If INTERMEDIATE is non-nil, also register for intermediate results.

CONTINUATIONS is a list of patterns with same syntax as `destructure-case'.
The result of the evaluation of SEXP is dispatched on CONTINUATIONS.
The result is either a sexp of the form (:ok VALUE) or (:error CONDITION).
CONTINUATIONS are executed asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 3))
  (let ((result (gensym)))
    `(let ,(cl-loop for var in saved-vars
                    collect (cl-etypecase var
                              (symbol (list var var))
                              (cons var)))
       (idris-dispatch-event
        (list :emacs-rex ,sexp
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations))
              ,@(when intermediate
                  `((lambda (,result)
                      (destructure-case ,result
                        ,@continuations)))))
        idris-connection))))

(defun idris-eval-async (sexp cont &optional failure-cont)
  "Evaluate SEXP on the superior Idris and call CONT or FAILURE-CONT."
  (idris-rex (cont (buffer (current-buffer)) failure-cont)
      sexp t
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:error condition &optional _spans)
     (when failure-cont
       (set-buffer buffer)
       (funcall failure-cont condition))
     (message "Evaluation returned an error: %s." condition))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar idris-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(autoload 'idris-list-compiler-notes "idris-commands.el")
(defun idris-eval (sexp &optional no-errors)
  "Evaluate SEXP on the inferior Idris and return the result.
If `NO-ERRORS' is non-nil, don't trigger warning buffers and
 don't call `ERROR' if there was an Idris error."
  (let* ((tag (gensym (format "idris-result-%d-"
                              (1+ idris-continuation-counter))))
         (idris-stack-eval-tags (cons tag idris-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (idris-rex (tag sexp)
           sexp nil
         ((:ok value &optional spans)
          (if (member tag idris-stack-eval-tags)
              (throw tag (list #'identity (cons value spans)))
            (if no-errors
                nil
              (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                     tag sexp))))
         ((:error condition &optional _spans)
          (if no-errors
              (throw tag (list #'identity nil))
            (when (member 'warnings-tree idris-warnings-printing)
              (idris-list-compiler-notes))
            (throw tag (list #'error "%s (synchronous Idris evaluation failed)" condition)))))
       (let ((debug-on-quit t)
             (inhibit-quit nil))
         (while t
           (when (eq (process-status idris-process) 'exit)
             (error "Idris process exited unexpectedly"))
           (accept-process-output idris-connection 0.1)))))))

(defvar idris-options-cache '()
  "An alist caching the Idris interpreter options.")

(defun idris-update-options-cache ()
  (idris-eval-async '(:get-options)
                    #'(lambda (opts) (setq idris-options-cache opts))))

(defun idris-get-options ()
  (idris-eval '(:get-options)))

(defun idris-get-option (opt)
  ;; First check the local cache
  (let ((local-val (assoc opt idris-options-cache)))
    (if local-val
        (equal (cadr local-val) :True)
      (let ((remote-val (assoc opt (car (idris-get-options)))))
        (if remote-val
            (equal (cadr remote-val) :True)
          (error "Unknown Idris option %s" opt))))))

(defun idris-set-option (opt b)
  (let ((bi (if b :True :False)))
    (idris-rex ((buffer (current-buffer)) opt b bi)
        `(:set-option ,opt ,bi) nil
      ((:ok _res)
       (set-buffer buffer)
       (let ((cache-elt (assoc opt idris-options-cache)))
         (if cache-elt
             (setf (cadr cache-elt) bi)
           (add-to-list 'idris-options-cache (list opt bi)))))
      ((:error condition &optional _spans)
       (message "Setting option %s to %s returned an error: %s." opt b condition)))))

(defun idris-get-idris-version ()
  "Ask the Idris compiler for its version information.
Returns a cons cell whose car is a list of version number
components and whose cdr is a list of prerelease identifiers, if applicable.
Returns nil if the version of Idris used doesn't support asking for versions."
  (pcase (idris-eval :version t)
    (`((,version ,prerelease)) (cons version prerelease))
    (_ nil)))

(defun idris-get-idris-version-string ()
  "Ask the Idris compiler for its version information.
Returns result as a user-friendly string.
Returns nil if the version of Idris used doesn't support asking for versions."
  (let ((version (idris-get-idris-version)))
    (if (consp version) ; returns nil on older versions of Idris
        (let* ((version-number (car version))
               (version-prerelease (cdr version)))
          (concat (mapconcat #'number-to-string version-number ".")
                  (if version-prerelease
                      (concat "-" (mapconcat #'identity version-prerelease "-"))
                    "")))
      nil)))


(provide 'inferior-idris)
;;; inferior-idris.el ends here

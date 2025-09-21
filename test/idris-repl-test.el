;;; idris-repl-test.el --- Tests for idris-repl  -*- lexical-binding: t -*-

(require 'ert)
(require 'idris-repl)

;; in order to use `idris-quit`
;; TODO: rewrite the test so it is not needed
(require 'idris-commands)

(ert-deftest idris-repl-buffer ()
  ;; Useful while debugging
  ;; (and (get-buffer idris-repl-buffer-name) (kill-buffer idris-repl-buffer-name))
  (let ((idris-repl-prompt-style 'short))
    (cl-flet ((idris-get-idris-version-stub () '((1 3 3))))
      (advice-add 'idris-get-idris-version :override #'idris-get-idris-version-stub)

      (unwind-protect
          (let* ((buffer (idris-repl-buffer)))
            (with-current-buffer buffer
              (display-buffer buffer)

              ;; Assert the short default prompt is present
              (should (string-match-p "λΠ> "
                                      (buffer-substring-no-properties (point-min) (point-max))))

              (with-selected-window (get-buffer-window)
                (should (eq (point) (point-max))))

              (idris-repl-update-prompt "TTT")
              (with-selected-window (get-buffer-window)
                (should (eq (point) (point-max))))

              ;; Assert the prompt did not change after update
              (should (string-match-p "λΠ> "
                                      (buffer-substring-no-properties (point-min) (point-max))))

              ;; when Idris repl prompt style is not 'short
              (let ((idris-repl-prompt-style 'long))
                (idris-repl-update-prompt "FooBar")

                (with-selected-window (get-buffer-window)
                  (should (eq (point) (point-max))))

                ;; Assert the prompt does change after update
                (should (string-match-p "FooBar> "
                                        (buffer-substring-no-properties (point-min) (point-max)))))

              ;; Cleanup
              (kill-buffer))
            (advice-remove 'idris-get-idris-version #'idris-get-idris-version-stub)
            (setq idris-prompt-string nil))))))

(ert-deftest idris-repl-event-hook-function ()
  (let* ((msg "We are about to implicitly bind the following lowercase names.")
         (event `(:warning
                  ("Flycheck.idr"
                   (4 2)
                   (4 3)
                   ,msg
                   ((186 17 ((:decor :type)))
                    (205 3 ((:decor :type)))
                    (235 3 ((:decor :type)))
                    (262 3 ((:decor :type)))
                    (268 3 ((:decor :type)))
                    (302 3 ((:decor :type)))
                    (328 1 ((:text-formatting :bold) (:decor :postulate)))))
                  69))
        (output (cadr event))
        (idris-warnings-printing '(warnings-repl warnings-tree)))
    ;; start repl and create connection to Idris
    (idris-repl)
    (idris-repl-event-hook-function event)
    (with-current-buffer "*idris-repl*"
      ;; Assert
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Flycheck.idr" str))
        (should (string-match-p msg str))))
    (idris-quit)))

;; https://github.com/idris-hackers/idris-mode/issues/443
(provide 'idris-repl-test)

;;; idris-repl-test.el ends here

;;; idris-info-test.el --- Tests related to Idris info buffer  -*- lexical-binding: t -*-
;; Copyright (C) 2022  Marek L.

;; Author: Marek L <nospam.keram@gmail.com>
;; Keywords: languages, Idris, help-mode, Ert

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

;;; Code:

(require 'ert)
(require 'idris-info)

(ert-deftest idris-show-info ()
  "Test displaying information in `idris-info-buffer'."
  (let ((info-str "Bool : Type")
        (info-spans '((0 4 ((:name "Prelude.Bool.Bool")
                            (:implicit :False)
                            (:key "AQA")
                            (:decor :type)
                            (:doc-overview "Boolean Data Type")
                            (:type "Type")
                            (:namespace "Prelude.Bool")))
                      (7 4 ((:decor :type)
                            (:type "Type")
                            (:doc-overview "The type of types")
                            (:name "Type")))
                      (7 4 ((:tt-term "AAA"))))))

    (idris-show-info info-str info-spans)

    (should (window-valid-p (get-buffer-window (get-buffer idris-info-buffer-name))))

    (with-current-buffer (get-buffer idris-info-buffer-name)
      (should (string-match-p info-str (buffer-string))))
    ;; TODO: Assert styling, context menu etc.

    ;; Cleanup
    (kill-buffer idris-info-buffer-name)))

(ert-deftest idris-info-buffer-history ()
  "Test displaying information in `idris-info-buffer'."
  (let ((info-str "First information")
        (info-str2 "Second information"))

    (idris-show-info info-str)

    (with-current-buffer (get-buffer idris-info-buffer-name)
      (should (string-match-p info-str (buffer-string)))
      (should (not (string-match-p "back" (buffer-string))))

      (idris-show-info info-str2)
      (should (string-match-p info-str2 (buffer-string)))
      (should (string-match-p "back" (buffer-string)))
      (help-go-back)

      (should (string-match-p info-str (buffer-string)))
      (should (string-match-p "forward" (buffer-string))))

    ;; Cleanup
    (kill-buffer idris-info-buffer-name)))

(provide 'idris-info-test)

;;; idris-info-test.el ends here

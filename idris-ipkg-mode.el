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
(require 'idris-common-utils)


;;; Faces

(defface idris-ipkg-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris package keywords"
  :group 'idris-faces)

(defface idris-ipkg-package-name-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to highlight the name of the package"
  :group 'idris-faces)


;;; Syntax

(defconst idris-ipkg-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    ;; Matching {}, but with nested comments
    (modify-syntax-entry ?\{ "(} 1bn" st)
    (modify-syntax-entry ?\} "){ 4bn" st)
    (modify-syntax-entry ?\- "_ 123" st)
    (modify-syntax-entry ?\n ">" st)

    st))

(defconst idris-ipkg-keywords
  '("package" "opts" "modules" "sourcedir" "makefile" "objs" "executable" "main" "libs"))

(defconst idris-ipkg-font-lock-defaults
  `(,idris-ipkg-keywords))


;;; Completion

(defun idris-ipkg-find-keyword ()
  (let ((start nil)
        (end (point))
        (failure (list nil nil nil)))
    (if (idris-is-ident-char-p (char-before))
        (progn
          (save-excursion
            (while (idris-is-ident-char-p (char-before))
              (backward-char))
            (setq start (point)))
          (if start
              (list (buffer-substring-no-properties start end)
                    start
                    end)
            failure))
      failure)))

(defun idris-ipkg-complete-keyword ()
  "Complete the current .ipkg keyword, if possible"
  (interactive)
  (cl-destructuring-bind (identifier start end) (idris-ipkg-find-keyword)
    (when identifier
      (list start end idris-ipkg-keywords))))

;;; Inserting fields
(defun idris-ipkg-insert-field ()
  "Insert one of the ipkg fields"
  (interactive)
  (let ((field (completing-read "Field: " (remove "package" idris-ipkg-keywords) nil t)))
    (beginning-of-line)
    (while (and (not (looking-at-p "^\\s-*$")) (= (forward-line) 0)))
    (beginning-of-line)
    (when (not (looking-at-p "^\\s-*$")) ;; end of buffer had stuff
      (goto-char (point-max))
      (newline))
    (newline)
    (insert field " = ")
    (let ((p (point)))
      (newline)
      (goto-char p))))

;;; Clickable modules

(defun idris-ipkg-make-files-clickable ()
  "Make all modules with existing files clickable, where clicking opens them"
  (interactive)
  (idris-clear-file-link-overlays)
  (let ((src-dir (idris-ipkg-buffer-src-dir (file-name-directory (buffer-file-name)))))
    ;; Make the sourcedir clickable
    (save-excursion
      (goto-char (point-min))
      (when (and (file-exists-p src-dir)
                 (file-directory-p src-dir)
                 (re-search-forward "^sourcedir\\s-*=\\s-*\\([a-zA-Z/0-9]+\\)" nil t))
        (let ((start (match-beginning 1))
              (end (match-end 1))
              (map (make-sparse-keymap)))
          (define-key map [mouse-2] #'(lambda ()
                                        (interactive)
                                        (dired src-dir)))
          (idris-make-file-link-overlay start end map
                                        (concat "mouse-2: dired " src-dir)))))
    ;; Make the modules clickable
    (save-excursion
      (goto-char (point-min))
      (cl-flet ((mod-link ()
                  (re-search-forward "[a-zA-Z0-9\\.]+" nil t)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (idris-make-module-link beg end src-dir))))
        (when (re-search-forward "^modules\\s-*=\\s-*" nil t)
          (cl-loop initially (mod-link)
                   while (looking-at-p "\\s-*,\\s-*")
                   do (progn (skip-chars-forward " ,\n")
                             (mod-link))))))
    ;; Make the Makefile clickable
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^makefile\\s-*=\\s-*\\([a-zA-Z/0-9]+\\)" nil t)
        (let ((start (match-beginning 1))
              (end (match-end 1))
              (makefile (concat (file-name-as-directory src-dir) (match-string 1))))
        (when (file-exists-p makefile)
          (let ((map (make-sparse-keymap)))
            (define-key map [mouse-2] #'(lambda ()
                                          (interactive)
                                          (find-file makefile)))
            (idris-make-file-link-overlay start end map  "mouse-2: edit makefile"))))))))


(defun idris-ipkg-enable-clickable-files ()
  "Enable setting up clickable modules and makefiles on idle Emacs"
  (interactive)
  (run-with-idle-timer 1 t 'idris-ipkg-make-files-clickable))

;;; finding ipkg files

;; Based on http://www.emacswiki.org/emacs/EmacsTags section "Finding tags files"
;; That page is GPL, so this is OK to include
(defun idris-find-file-upwards (suffix &optional allow-hidden)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name ending in suffix.  Returns the paths
to the matching files, or nil if not found."
  (cl-labels
      ((find-file-r (path)
         (let* ((parent (file-name-directory path))
                (matching (directory-files parent t (concat suffix "$"))))
           (cond
            (matching matching)
            ;; The parent of ~ is nil and the parent of / is itself.
            ;; Thus the terminating condition for not finding the file
            ;; accounts for both.
            ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
            (t (find-file-r (directory-file-name parent))))))) ; Continue
    (cl-remove-if #'(lambda (f)
                      (and (not allow-hidden)
                           (string-prefix-p "." (file-name-nondirectory f))))
                  (find-file-r default-directory))))

(defvar idris-ipkg-build-buffer-name "*idris-build*")

(defvar idris-ipkg-build-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map) ; remove the self-inserting char commands
    (define-key map (kbd "q") 'idris-ipkg-build-quit)
    map))

(easy-menu-define idris-ipkg-build-mode-menu idris-ipkg-build-mode-map
  "Menu for the Idris build mode buffer"
  `("Idris Building"
    ["Close Idris build buffer" idris-ipkg-build-quit t]))

(define-derived-mode idris-ipkg-build-mode fundamental-mode "Idris Build"
  "Major mode used for transient Idris build bufers
    \\{idris-ipkg-build-mode-map}
Invokes `idris-ipkg-build-mode-hook'.")

(defun idris-ipkg-command (ipkg-file command)
  "Run a command on ipkg-file. The command can be build, install, or clean."
  ;; Idris must have its working directory in the same place as the ipkg file
  (let ((dir (file-name-directory ipkg-file))
        (file (file-name-nondirectory ipkg-file))
        (cmd (cond ((equal command 'build) "--build")
                    ((equal command 'install) "--install")
                    ((equal command 'clean) "--clean")
                    (t (error "Invalid command name %s" command)))))
    (unless dir
      (error "Unable to determine directory for filename '%s'" ipkg-file))
    (let ((default-directory dir)) ; default-directory is a special variable - this starts idris in dir
      (start-process (concat "idris " cmd)
                     idris-ipkg-build-buffer-name idris-interpreter-path cmd file)
      (with-current-buffer idris-ipkg-build-buffer-name
        (idris-ipkg-build-mode))
      (pop-to-buffer idris-ipkg-build-buffer-name))))

(defun idris-ipkg-build (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to build: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to build: " nil nil nil t)))))
  (idris-ipkg-command ipkg-file 'build))

(defun idris-ipkg-install (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to install: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to install: " nil nil nil t)))))
  (idris-ipkg-command ipkg-file 'install))

(defun idris-ipkg-clean (ipkg-file)
  (interactive (list
                (let ((ipkg-default (idris-find-file-upwards "ipkg")))
                  (if ipkg-default
                      (read-file-name "Package file to clean: "
                                      (file-name-directory (car ipkg-default))
                                      (car ipkg-default)
                                      t
                                      (file-name-nondirectory (car ipkg-default)))
                    (read-file-name "Package file to clean: " nil nil nil t)))))
  (idris-ipkg-command ipkg-file 'clean))

(defun idris-ipkg-build-quit ()
  (interactive)
  (idris-kill-buffer idris-ipkg-build-buffer-name))

(defun idris-ipkg-buffer-src-dir (basename)
  (save-excursion
    (goto-char (point-min))
    (let ((found
           (re-search-forward "^\\s-*sourcedir\\s-*=\\s-*\\(\\sw+\\)"
                              nil
                              t)))
      (if found
          (let ((subdir (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
            (concat (file-name-directory basename) subdir))
        (file-name-directory basename)))))

(defun idris-ipkg-find-src-dir (&optional ipkg-file)
  (let ((found (or (and ipkg-file (list ipkg-file))
                   (idris-find-file-upwards "ipkg"))))
    (if (not found)
        nil
      (setq ipkg-file (car found))
      ;; Now ipkg-file contains the path to the package
      (with-temp-buffer
        (insert-file-contents ipkg-file)
        (idris-ipkg-buffer-src-dir ipkg-file)))))


;;; Settings

(defgroup idris-ipkg nil "Idris package mode" :prefix 'idris-ipkg :group 'idris)

(defcustom idris-ipkg-mode-hook '(idris-ipkg-enable-clickable-files)
  "Functions to call when opening the Idris package mode"
  :type 'hook
  :options '(idris-ipkg-enable-clickable-files)
  :group 'idris-ipkg)

;;; Mode definition

(defvar idris-ipkg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b b") 'idris-ipkg-build)
    (define-key map (kbd "C-c C-b C-b") 'idris-ipkg-build)
    (define-key map (kbd "C-c C-b c") 'idris-ipkg-clean)
    (define-key map (kbd "C-c C-b C-c") 'idris-ipkg-clean)
    (define-key map (kbd "C-c C-b i") 'idris-ipkg-install)
    (define-key map (kbd "C-c C-b C-i") 'idris-ipkg-install)
    (define-key map (kbd "C-c C-f") 'idris-ipkg-insert-field)
    map)
  "Keymap used for Idris package mode")

(easy-menu-define idris-ipkg-mode-menu idris-ipkg-mode-map
  "Menu for Idris package mode"
  `("IPkg"
    ["Build package" idris-ipkg-build t]
    ["Install package" idris-ipkg-install t]
    ["Clean package" idris-ipkg-clean t]
    "----------------"
    ["Insert field" idris-ipkg-insert-field t]))

;;;###autoload
(define-derived-mode idris-ipkg-mode prog-mode "Idris Pkg"
  "Major mode for Idris package files
     \\{idris-ipkg-mode-map}
Invokes `idris-ipkg-mode-hook'."
  :group 'idris
  :syntax-table idris-ipkg-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       idris-ipkg-font-lock-defaults)
  (set (make-local-variable 'completion-at-point-functions)
       '(idris-ipkg-complete-keyword)))

(push '("\\.ipkg$" . idris-ipkg-mode) auto-mode-alist)

(provide 'idris-ipkg-mode)

;;; idris-ipkg-mode.el ends here

;;; plantuml-mode.el --- Major mode for plantuml

;; Author: Zhang Weize (zwz)
;; Keywords: uml ascii
;; Version: 0.2

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2
;; NOTE: licensing fixed to GPLv2 as per original author comment

;;; DESCRIPTION

;; A major mode for plantuml, see: http://plantuml.sourceforge.net/
;; Plantuml is an open-source tool in java that allows to quickly write :
;;     - sequence diagram,
;;     - use case diagram,
;;     - class diagram,
;;     - activity diagram,
;;     - component diagram,
;;     - state diagram
;;     - object diagram
;; using a simple and intuitive language.

;;; HISTORY
;; version 0.2, 2010-09-20 Initialize the keywords from the -language output of plantuml.jar instead of the hard-coded way.
;; version 0.1, 2010-08-25 First version


(require 'thingatpt)

(defgroup plantuml-mode nil
  "Major mode for editing plantuml file."
  :group 'languages)

(defvar plantuml-jar-path (expand-file-name "~/plantuml.jar"))

(defvar plantuml-mode-hook nil "Standard hook for plantuml-mode.")

(defvar plantuml-mode-version nil "plantuml-mode version string.")

(defvar plantuml-mode-map nil "Keymap for plantuml-mode")

;;; syntax table
(defvar plantuml-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?' "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    (modify-syntax-entry ?! "w" synTable)
    (modify-syntax-entry ?@ "w" synTable)
    (modify-syntax-entry ?# "'" synTable)
    synTable)
  "Syntax table for `plantuml-mode'.")

(defvar plantuml-types nil)
(defvar plantuml-keywords nil)
(defvar plantuml-preprocessors nil)
(defvar plantuml-builtins nil)

;; keyword completion
(defvar plantuml-kwdList nil "plantuml keywords.")

;;; font-lock

(defun plantuml-init ()
  "Initialize the keywords or builtins from the cmdline language output"
  (unless (file-exists-p plantuml-jar-path)
    (error "Could not find plantuml.jar at %s" plantuml-jar-path))
  (with-temp-buffer
    (shell-command (concat "java -jar "
                           (shell-quote-argument plantuml-jar-path)
                           " -language") (current-buffer))
    (goto-char (point-min))
    (let ((found (search-forward ";" nil nil))
          (word "")
          (count 0)
          (pos 0))
      (while found
        (forward-char)
        (setq word (current-word))
        (if (string= word "EOF") (setq found nil)
            ;; else
            (forward-line)
            (setq count (string-to-number (current-word)))
            (beginning-of-line 2)
            (setq pos (point))
            (forward-line count)
            (cond ((string= word "type")
                   (setq plantuml-types
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  ((string= word "keyword")
                   (setq plantuml-keywords
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  ((string= word "preprocessor")
                   (setq plantuml-preprocessors
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  (t (setq plantuml-builtins
                           (append
                            plantuml-builtins
                            (split-string
                             (buffer-substring-no-properties pos (point)))))))
;;                  ((string= word "skinparameter")
;;                  ((string= word "color")))
            (setq found (search-forward ";" nil nil)))))))

(unless plantuml-kwdList
  (plantuml-init)
  (defvar plantuml-types-regexp (concat "^\\s *\\(" (regexp-opt plantuml-types 'words) "\\|\\<\\(note\\s +over\\|note\\s +\\(left\\|right\\|bottom\\|top\\)\\s +\\(of\\)?\\)\\>\\|\\<\\(\\(left\\|center\\|right\\)\\s +\\(header\\|footer\\)\\)\\>\\)"))
  (defvar plantuml-keywords-regexp (concat "^\\s *" (regexp-opt plantuml-keywords 'words)  "\\|\\(<\\|<|\\|\\*\\|o\\)\\(\\.+\\|-+\\)\\|\\(\\.+\\|-+\\)\\(>\\||>\\|\\*\\|o\\)\\|\\.\\{2,\\}\\|-\\{2,\\}"))
  (defvar plantuml-builtins-regexp (regexp-opt plantuml-builtins 'words))
  (defvar plantuml-preprocessors-regexp (concat "^\\s *" (regexp-opt plantuml-preprocessors 'words)))

  (setq plantuml-font-lock-keywords
        `(
          (,plantuml-types-regexp . font-lock-type-face)
          (,plantuml-keywords-regexp . font-lock-keyword-face)
          (,plantuml-builtins-regexp . font-lock-builtin-face)
          (,plantuml-preprocessors-regexp . font-lock-preprocessor-face)
          ;; note: order matters
          ))



  (setq plantuml-kwdList (make-hash-table :test 'equal))
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-types)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-keywords)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-builtins)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-preprocessors)
  (put 'plantuml-kwdList 'risky-local-variable t)

  ;; clear memory
  (setq plantuml-types nil)
  (setq plantuml-keywords nil)
  (setq plantuml-builtins nil)
  (setq plantuml-preprocessors nil)
  (setq plantuml-types-regexp nil)
  (setq plantuml-keywords-regexp nil)
  (setq plantuml-builtins-regexp nil)
  (setq plantuml-preprocessors-regexp nil))

(defun plantuml-complete-symbol ()
  "Perform keyword completion on word before cursor."
  (interactive)
  (let ((posEnd (point))
        (meat (thing-at-point 'symbol))
        maxMatchResult)

    (when (not meat) (setq meat ""))

    (setq maxMatchResult (try-completion meat plantuml-kwdList))
    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for \"%s\"" meat)
           (ding))
          ((not (string= meat maxMatchResult))
           (delete-region (- posEnd (length meat)) posEnd)
           (insert maxMatchResult))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list
                (all-completions meat plantuml-kwdList)
                meat))
             (message "Making completion list...%s" "done")))))

(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))

;;;###autoload
(defun plantuml-mode ()
  "Major mode for plantuml.

Shortcuts             Command Name
\\[plantuml-complete-symbol]      `plantuml-complete-symbol'"

  (interactive)
  (kill-all-local-variables)

;;  (python-mode) ; for indentation
  (setq major-mode 'plantuml-mode
        mode-name "plantuml")
  (set-syntax-table plantuml-mode-syntax-table)
  (use-local-map plantuml-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((plantuml-font-lock-keywords) nil t))

  (run-mode-hooks 'plantuml-mode-hook))

(provide 'plantuml-mode)
;;; plantuml-mode.el ends here
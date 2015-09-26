;;; puml-mode.el --- Major mode for PlantUML

;; Filename: puml-mode.el
;; Description: Major mode for PlantUML diagrams sources
;; Compatibility: Tested with Emacs 24.3 through 24.5 on OS X 10.10
;; Author: Zhang Weize (zwz)
;; Maintainer: Carlo Sciolla (skuro)
;; Keywords: uml plantuml ascii
;; Version: 0.6

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2
;; NOTE: licensing fixed to GPLv2 as per original author comment

;;; Commentary:
;;
;; A major mode for plantuml, see: http://plantuml.sourceforge.net/
;; Plantuml is an open-source tool in java that allows to quickly write :
;;     - sequence diagram,
;;     - use case diagram,
;;     - class diagram,
;;     - activity diagram,
;;     - component diagram,
;;     - state diagram
;;     - object diagram

;;; Change log:
;;
;; version 0.6.1, 2015-09-26 Bugfix: use eq to compare symbols instead of cl-equalp
;; version 0.6, 2015-09-26 Fixed PNG preview
;; version 0.5, 2015-09-21 Added preview capabilities
;; version 0.4, 2015-06-14 Use a puml- prefix to distinguish from the other plantuml-mode
;; version 0.3, 2015-06-13 Compatibility with Emacs 24.x
;; version 0.2, 2010-09-20 Initialize the keywords from the -language output of plantuml.jar instead of the hard-coded way.
;; version 0.1, 2010-08-25 First version

;;; Code:
(require 'thingatpt)

(defgroup puml-mode nil
  "Major mode for editing plantuml file."
  :group 'languages)

(defcustom puml-plantuml-jar-path
  (expand-file-name "~/plantuml.jar")
  "The location of the PlantUML executable JAR.")

(defvar puml-mode-hook nil "Standard hook for puml-mode.")

(defvar puml-mode-version "0.6.1" "The puml-mode version string.")

(defvar puml-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-c C-c") 'puml-preview)
    keymap)
  "Keymap for puml-mode.")

;;; syntax table
(defvar puml-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?' "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    (modify-syntax-entry ?! "w" synTable)
    (modify-syntax-entry ?@ "w" synTable)
    (modify-syntax-entry ?# "'" synTable)
    synTable)
  "Syntax table for `puml-mode'.")

(defvar puml-plantuml-types nil)
(defvar puml-plantuml-keywords nil)
(defvar puml-plantuml-preprocessors nil)
(defvar puml-plantuml-builtins nil)

;; keyword completion
(defvar puml-plantuml-kwdList nil "The plantuml keywords.")

;;; font-lock

(defun puml-init ()
  "Initialize the keywords or builtins from the cmdline language output."
  (unless (file-exists-p puml-plantuml-jar-path)
    (error "Could not find plantuml.jar at %s" puml-plantuml-jar-path))
  (with-temp-buffer
    (shell-command (concat "java -jar "
                           (shell-quote-argument puml-plantuml-jar-path)
                           " -language") (current-buffer))
    (goto-char (point-min))
    (let ((found (search-forward ";" nil t))
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
                   (setq puml-plantuml-types
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  ((string= word "keyword")
                   (setq puml-plantuml-keywords
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  ((string= word "preprocessor")
                   (setq puml-plantuml-preprocessors
                     (split-string
                      (buffer-substring-no-properties pos (point)))))
                  (t (setq puml-plantuml-builtins
                           (append
                            puml-plantuml-builtins
                            (split-string
                             (buffer-substring-no-properties pos (point)))))))
;;                  ((string= word "skinparameter")
;;                  ((string= word "color")))
            (setq found (search-forward ";" nil nil)))))))

(defconst puml-preview-buffer "*PUML Preview*")

(defun puml-output-type ()
  "Detects the best output type to use for generated diagrams."
  (cond ((image-type-available-p 'svg) 'svg)
        ((image-type-available-p 'png) 'png)
        ('utxt)))

(defun puml-is-image-output-p ()
  "Return true if the diagram output format is an image, false if it's text based."
  (not (eq 'utxt (puml-output-type))))

(defun puml-output-type-opt ()
  "Create the flag to pass to PlantUML to produce the selected output format."
  (let ((type (puml-output-type)))
    (concat "-t" (symbol-name type))))

(defun puml-preview-sentinel (ps event)
  "For the PlantUML process (as PS) reacts on the termination event (as EVENT)."
  (if (equal event "finished\n")
      (progn
        (switch-to-buffer puml-preview-buffer)
        (when (and (display-images-p)
                   (puml-is-image-output-p))
          (image-mode)))
    (warn "PUML Preview failed: %s" event)))

(defun puml-preview ()
  "Preview diagram."
  (interactive)
  (let ((b (get-buffer puml-preview-buffer)))
    (when b
      (kill-buffer b)))
  (let ((process-connection-type nil)
        (buf (get-buffer-create puml-preview-buffer))
        (coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (let ((ps (start-process "PUML" buf
                             "java" "-jar" (shell-quote-argument puml-plantuml-jar-path)
                             (puml-output-type-opt) "-p")))
      (process-send-region ps (point-min) (point-max))
      (process-send-eof ps)
      (set-process-sentinel ps 'puml-preview-sentinel))))

(unless puml-plantuml-kwdList
  (puml-init)
  (defvar puml-plantuml-types-regexp (concat "^\\s *\\(" (regexp-opt puml-plantuml-types 'words) "\\|\\<\\(note\\s +over\\|note\\s +\\(left\\|right\\|bottom\\|top\\)\\s +\\(of\\)?\\)\\>\\|\\<\\(\\(left\\|center\\|right\\)\\s +\\(header\\|footer\\)\\)\\>\\)"))
  (defvar puml-plantuml-keywords-regexp (concat "^\\s *" (regexp-opt puml-plantuml-keywords 'words)  "\\|\\(<\\|<|\\|\\*\\|o\\)\\(\\.+\\|-+\\)\\|\\(\\.+\\|-+\\)\\(>\\||>\\|\\*\\|o\\)\\|\\.\\{2,\\}\\|-\\{2,\\}"))
  (defvar puml-plantuml-builtins-regexp (regexp-opt puml-plantuml-builtins 'words))
  (defvar puml-plantuml-preprocessors-regexp (concat "^\\s *" (regexp-opt puml-plantuml-preprocessors 'words)))

  (setq puml-font-lock-keywords
        `(
          (,puml-plantuml-types-regexp . font-lock-type-face)
          (,puml-plantuml-keywords-regexp . font-lock-keyword-face)
          (,puml-plantuml-builtins-regexp . font-lock-builtin-face)
          (,puml-plantuml-preprocessors-regexp . font-lock-preprocessor-face)
          ;; note: order matters
          ))



  (setq puml-plantuml-kwdList (make-hash-table :test 'equal))
  (mapc (lambda (x) (puthash x t puml-plantuml-kwdList)) puml-plantuml-types)
  (mapc (lambda (x) (puthash x t puml-plantuml-kwdList)) puml-plantuml-keywords)
  (mapc (lambda (x) (puthash x t puml-plantuml-kwdList)) puml-plantuml-builtins)
  (mapc (lambda (x) (puthash x t puml-plantuml-kwdList)) puml-plantuml-preprocessors)
  (put 'puml-plantuml-kwdList 'risky-local-variable t)

  ;; clear memory
  (setq puml-plantuml-types nil)
  (setq puml-plantuml-keywords nil)
  (setq puml-plantuml-builtins nil)
  (setq puml-plantuml-preprocessors nil)
  (setq puml-plantuml-types-regexp nil)
  (setq puml-plantuml-keywords-regexp nil)
  (setq puml-plantuml-builtins-regexp nil)
  (setq puml-plantuml-preprocessors-regexp nil))

(defun puml-complete-symbol ()
  "Perform keyword completion on word before cursor."
  (interactive)
  (let ((posEnd (point))
        (meat (thing-at-point 'symbol))
        maxMatchResult)

    (when (not meat) (setq meat ""))

    (setq maxMatchResult (try-completion meat puml-plantuml-kwdList))
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
                (all-completions meat puml-plantuml-kwdList)))
             (message "Making completion list...%s" "done")))))

(add-to-list 'auto-mode-alist '("\\.pum$" . puml-mode))

;;;###autoload
(define-derived-mode puml-mode prog-mode "puml"
  "Major mode for plantuml.

Shortcuts             Command Name
\\[puml-complete-symbol]      `puml-complete-symbol'"
  (setq font-lock-defaults '((puml-font-lock-keywords) nil t)))

(provide 'puml-mode)
;;; puml-mode.el ends here

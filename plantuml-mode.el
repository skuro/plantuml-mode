;;; plantuml-mode.el --- Major mode for PlantUML    -*- lexical-binding: t; -*-

;; Filename: plantuml-mode.el
;; Description: Major mode for PlantUML diagrams sources
;; Compatibility: Tested with Emacs 24.3 through 24.5 on OS X 10.10
;; Author: Zhang Weize (zwz)
;; Maintainer: Carlo Sciolla (skuro)
;; Keywords: uml plantuml ascii
;; Version: 1.2.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
;; version 1.2.5, 2017-08-19 #53 Fixed installation warnings
;; version 1.2.4, 2017-08-18 #60 Licensed with GPLv3+ to be compatible with Emacs
;; version 1.2.3, 2016-12-25 #50 unicode support in generated output
;; version 1.2.2, 2016-11-11 Fixed java commands handling under windows; support spaces in `plantuml-jar-path'
;; version 1.2.1, 2016-11-11 Support for paths like `~/.plantuml/plantuml.jar' for `plantuml-jar-path' (the tilde was previously unsupported)
;; version 1.2.0, 2016-11-09 Added `plantuml-preview-current-buffer', courtesy of @7mamu4
;; version 1.1.1, 2016-11-08 Fix process handling with Windows native emacs; better file extention match for autoloading the mode
;; version 1.1.0, 2016-10-18 Make PlantUML run headless by default; introduced custom variable `plantuml-java-args' to control which arguments are passed to Plantuml.
;; version 1.0.1, 2016-10-17 Bugfix release: proper auto-mode-alist regex; init delayed at mode load; avoid calling hooks twice.
;; version 1.0.0, 2016-10-16 Moved the mode to plantuml-mode, superseding zwz/plantuml-mode and skuro/puml-mode. Added preview for the currently selected region.
;; version 0.6.7, 2016-10-11 [from puml-mode] Added deprecation warning in favor of plantuml-mode
;; version 0.6.6, 2016-07-19 [from puml-mode] Added autoload, minor bug fixes
;; version 0.6.5, 2016-03-24 [from puml-mode] Added UTF8 support and open in new window / frame shortcuts
;; version 0.6.4, 2015-12-12 [from puml-mode] Added support for comments (single and multiline) -- thanks to https://github.com/nivekuil
;; version 0.6.3, 2015-11-07 [from puml-mode] Added per-buffer configurability of output type (thanks to https://github.com/davazp)
;; version 0.6.2, 2015-11-07 [from puml-mode] Added debugging capabilities to improve issue analysis
;; version 0.6.1, 2015-09-26 [from puml-mode] Bugfix: use eq to compare symbols instead of cl-equalp
;; version 0.6, 2015-09-26 [from puml-mode] Fixed PNG preview
;; version 0.5, 2015-09-21 [from puml-mode] Added preview capabilities
;; version 0.4, 2015-06-14 [from puml-mode] Use a puml- prefix to distinguish from the other plantuml-mode
;; version 0.3, 2015-06-13 [from puml-mode] Compatibility with Emacs 24.x
;; version 0.2, 2010-09-20 [from puml-mode] Initialize the keywords from the -language output of plantuml.jar instead of the hard-coded way.
;; version 0.1, 2010-08-25 [from puml-mode] First version

;;; Code:
(require 'thingatpt)

(defgroup plantuml-mode nil
  "Major mode for editing plantuml file."
  :group 'languages)

(defcustom plantuml-jar-path
  (expand-file-name "~/plantuml.jar")
  "The location of the PlantUML executable JAR.")

(defvar plantuml-mode-hook nil "Standard hook for plantuml-mode.")

(defconst plantuml-mode-version "1.2.3" "The plantuml-mode version string.")

(defvar plantuml-mode-debug-enabled nil)

(defvar plantuml-font-lock-keywords nil)

(defvar plantuml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") 'plantuml-preview)
    keymap)
  "Keymap for plantuml-mode.")

(defcustom plantuml-java-command "java"
  "The java command used to execute PlantUML.")

(eval-and-compile
  (defcustom plantuml-java-args '("-Djava.awt.headless=true" "-jar")
    "The parameters passed to `plantuml-java-command' when executing PlantUML."))

(defcustom plantuml-suppress-deprecation-warning t
  "To silence the deprecation warning when `puml-mode' is found upon loading.")

(defun plantuml-render-command (&rest arguments)
  "Create a command line to execute PlantUML with arguments (as ARGUMENTS)."
  (let* ((cmd-list (append plantuml-java-args (list (expand-file-name plantuml-jar-path)) arguments))
         (cmd (mapconcat 'identity cmd-list "|")))
    (plantuml-debug (format "Command is [%s]" cmd))
    cmd-list))

;;; syntax table
(defvar plantuml-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/  ". 14c"   synTable)
    (modify-syntax-entry ?'   "< 23"    synTable)
    (modify-syntax-entry ?\n  ">"       synTable)
    (modify-syntax-entry ?\r  ">"       synTable)
    (modify-syntax-entry ?!   "w"       synTable)
    (modify-syntax-entry ?@   "w"       synTable)
    (modify-syntax-entry ?#   "'"       synTable)
    synTable)
  "Syntax table for `plantuml-mode'.")

(defvar plantuml-types nil)
(defvar plantuml-keywords nil)
(defvar plantuml-preprocessors nil)
(defvar plantuml-builtins nil)

;; keyword completion
(defvar plantuml-kwdList nil "The plantuml keywords.")

(defun plantuml-enable-debug ()
  "Enables debug messages into the *PLANTUML Messages* buffer."
  (interactive)
  (setq plantuml-mode-debug-enabled t))

(defun plantuml-disable-debug ()
  "Stops any debug messages to be added into the *PLANTUML Messages* buffer."
  (interactive)
  (setq plantuml-mode-debug-enabled nil))

(defun plantuml-debug (msg)
  "Writes msg (as MSG) into the *PLANTUML Messages* buffer without annoying the user."
  (if plantuml-mode-debug-enabled
      (let* ((log-buffer-name "*PLANTUML Messages*")
             (log-buffer (get-buffer-create log-buffer-name)))
        (save-excursion
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert msg)
            (insert "\n"))))))

(defun plantuml-init ()
  "Initialize the keywords or builtins from the cmdline language output."
  (unless (or (eq system-type 'cygwin) (file-exists-p plantuml-jar-path))
    (error "Could not find plantuml.jar at %s" plantuml-jar-path))
  (with-temp-buffer
    (let ((cmd-args (append (list plantuml-java-command nil t nil)
                            (plantuml-render-command "-charset" "UTF-8" "-language"))))
      (apply 'call-process cmd-args)
      (goto-char (point-min)))
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
          (setq found (search-forward ";" nil nil)))))))

(defconst plantuml-preview-buffer "*PLANTUML Preview*")

(defvar plantuml-output-type
  (if (not (display-images-p))
      "utxt"
    (cond ((image-type-available-p 'svg) "svg")
          ((image-type-available-p 'png) "png")
          (t "utxt")))
  "Specify the desired output type to use for generated diagrams.")

(defun plantuml-read-output-type ()
  "Read from the minibuffer a output type."
  (let* ((completion-ignore-case t)
         (available-types
          (append
           (and (image-type-available-p 'svg) '("svg"))
           (and (image-type-available-p 'png) '("png"))
           '("utxt"))))
    (completing-read (format "Output type [%s]: " plantuml-output-type)
                     available-types
                     nil
                     t
                     nil
                     nil
                     plantuml-output-type)))

(defun plantuml-set-output-type (type)
  "Set the desired output type (as TYPE) for the current buffer.
If the
major mode of the current buffer mode is not plantuml-mode, set the
default output type for new buffers."
  (interactive (list (plantuml-read-output-type)))
  (setq plantuml-output-type type))

(defun plantuml-is-image-output-p ()
  "Return true if the diagram output format is an image, false if it's text based."
  (not (equal "utxt" plantuml-output-type)))

(defun plantuml-output-type-opt ()
  "Create the flag to pass to PlantUML to produce the selected output format."
  (concat "-t" plantuml-output-type))

(defmacro plantuml-start-process (buf)
  "Run PlantUML as an Emacs process and puts the output into the given buffer (as BUF)."
  `(start-process "PLANTUML" ,buf
                  plantuml-java-command
                  ,@plantuml-java-args
                  (expand-file-name plantuml-jar-path)
                  (plantuml-output-type-opt) "-charset" "UTF-8" "-p"))

(defun plantuml-preview-string (prefix string)
  "Preview diagram from PlantUML sources (as STRING), using prefix (as PREFIX)
to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (let ((b (get-buffer plantuml-preview-buffer)))
    (when b
      (kill-buffer b)))

  (let* ((imagep (and (display-images-p)
                      (plantuml-is-image-output-p)))
         (process-connection-type nil)
         (buf (get-buffer-create plantuml-preview-buffer))
         (coding-system-for-read (and imagep 'binary))
         (coding-system-for-write (and imagep 'binary)))

    (let ((ps (plantuml-start-process buf)))
      (process-send-string ps string)
      (process-send-eof ps)
      (set-process-sentinel ps
                            (lambda (_ps event)
                              (unless (equal event "finished\n")
                                (error "PLANTUML Preview failed: %s" event))
                              (cond
                               ((= prefix 16)
                                (switch-to-buffer-other-frame plantuml-preview-buffer))
                               ((= prefix 4)
                                (switch-to-buffer-other-window plantuml-preview-buffer))
                               (t (switch-to-buffer plantuml-preview-buffer)))
                              (when imagep
                                (image-mode)
                                (set-buffer-multibyte t)))))))

(defun plantuml-preview-buffer (prefix)
  "Preview diagram from the PlantUML sources in the current buffer.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (plantuml-preview-string prefix (buffer-string)))

(defun plantuml-preview-region (prefix begin end)
  "Preview diagram from the PlantUML sources in from BEGIN to END.
Uses the current region when called interactively.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p\nr")
  (plantuml-preview-string prefix (concat "@startuml\n"
                                      (buffer-substring-no-properties
                                       begin end)
                                      "\n@enduml")))

(defun plantuml-preview-current-block (prefix)
  "Preview diagram from the PlantUML sources from the previous @startuml to the next @enduml.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (search-backward "@startuml") (search-forward "@enduml"))
    (plantuml-preview-buffer prefix)))

(defun plantuml-preview (prefix)
  "Preview diagram from the PlantUML sources.
Uses the current region if one is active, or the entire buffer otherwise.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  (if mark-active
      (plantuml-preview-region prefix (region-beginning) (region-end))
      (plantuml-preview-buffer prefix)))

(defun plantuml-init-once ()
  "Ensure initialization only happens once."
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
    (setq plantuml-preprocessors-regexp nil)))

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
                (all-completions meat plantuml-kwdList)))
             (message "Making completion list...%s" "done")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(plantuml\\|pum\\|plu\\)\\'" . plantuml-mode))

;;;###autoload
(define-derived-mode plantuml-mode prog-mode "plantuml"
  "Major mode for plantuml.

Shortcuts             Command Name
\\[plantuml-complete-symbol]      `plantuml-complete-symbol'"
  (plantuml-init-once)
  (make-local-variable 'plantuml-output-type)
  (set (make-local-variable 'comment-start-skip) "\\('+\\|/'+\\)\\s *")
  (set (make-local-variable 'comment-start) "/'")
  (set (make-local-variable 'comment-end) "'/")
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-style) 'extra-line)
  (setq font-lock-defaults '((plantuml-font-lock-keywords) nil t)))

(defun plantuml-deprecation-warning ()
  "Warns the user about the deprecation of the `puml-mode' project."
  (if (and plantuml-suppress-deprecation-warning
           (featurep 'puml-mode))
      (display-warning :warning
                       "`puml-mode' is now deprecated and no longer updated, but it's still present in your system.\
You should move your configuration to use `plantuml-mode'. See https://github.com/sytac/plantuml-mode. \
See more at https://github.com/skuro/puml-mode/issues/26")))

(add-hook 'plantuml-mode-hook 'plantuml-deprecation-warning)

(provide 'plantuml-mode)
;;; plantuml-mode.el ends here

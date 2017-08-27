;;; test-helper.el --- PlantUML Mode test initialization   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(require 'f)

(defvar package-test-path
  (f-dirname (f-this-file)))

(defvar package-code-path
  (f-parent package-test-path))

(defvar plantuml-test-resources-path
  (f-join package-code-path "test/resources"))

(defvar plantuml-test-jar-path
  (f-join package-code-path "bin/plantuml.jar"))

(defun test-file-path (path)
  "Translate the relative test path PATH in an absolute path."
  (f-join plantuml-test-resources-path path))

(defun cleanup-preview ()
  "Kill the preview buffer"
  (let ((proc (get-buffer-process plantuml-preview-buffer)))
    (when proc
      (set-process-query-on-exit-flag proc nil))
    (kill-buffer plantuml-preview-buffer)))

(defun read-buffer (bufname)
  "Read the contents of buffer BUFNAME."
  (with-current-buffer (get-buffer bufname)
    (buffer-string)))

(defun read-preview-buffer ()
  "Read the contents of the PlantUML preview buffer."
  (read-buffer plantuml-preview-buffer))

(defun read-test-file (path)
  "Fetch the string content of the test file at relative path PATH."
  (f-read (test-file-path path) 'utf-8))

(defun open-test-file-in-buf (test-file)
  "Visit TEST-FILE in a new buffer."
  (message "buffer list before: %s" (buffer-list))
  (find-file (test-file-path test-file))
  (message "buffer list after: %s" (buffer-list)))

(defun load-plantuml-mode ()
  "Load the plantuml-mode package."
  (require 'plantuml-mode (f-expand "plantuml-mode.el" package-code-path)))

(load-plantuml-mode)

;;; test-helper.el ends here

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

(defun read-buffer (bufname)
  (with-current-buffer (get-buffer bufname)
    (buffer-string)))

(defun read-preview-buffer ()
  (read-buffer plantuml-preview-buffer))

(defun read-test-file (path)
  (f-read (f-join plantuml-test-resources-path path) 'utf-8))

(defun load-plantuml-mode ()
  (require 'plantuml-mode (f-expand "plantuml-mode.el" package-code-path)))

(defun format-preview-output (s)
  "Make the preview output as S more readable in test output."
  (concat "\n" s))

(defun plantuml-test-indent-block (before after)
  "The common code for the block indentation tests.

BEFORE is the text block to be inserted into a temporary buffer.
AFTER is the expected text block after indentation.

The temporary buffer will be put into `plantuml-mode'. The whole buffer
will be indented with two spaces for each level of indentation.

Finally, the indented text in the buffer will be compared with AFTER."

  (with-temp-buffer
    ;; fix the JAR location prior to mode initialization
    ;; for some reason, plantuml-mode disregards the setq-local
    (setq plantuml-jar-path plantuml-test-jar-path)
    (plantuml-init-once 'jar)

    (insert before)
    (goto-char (point-min))
    (plantuml-mode)
    ;; use 2 spaces instead of one tab for indentation
    (setq indent-tabs-mode nil)
    (setq tab-width 2)

    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) after))))

;; enable code coverage
(when (require 'undercover nil t)
  (undercover "plantuml-mode.el"))

(load-plantuml-mode)

;;; test-helper.el ends here

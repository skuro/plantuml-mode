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

(require 'plantuml-mode (f-expand "plantuml-mode.el" package-code-path))

;;; test-helper.el ends here

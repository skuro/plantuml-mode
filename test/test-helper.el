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

(require 'plantuml-mode (f-expand "plantuml-mode.el" package-code-path))

;;; test-helper.el ends here

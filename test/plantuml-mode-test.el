;;; plantuml-mode-test.el --- PlantUML Mode tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(ert-deftest initial-jar-location ()
  (should (equal (expand-file-name "~/plantuml.jar")
                 plantuml-jar-path)))

(provide 'plantuml-mode-test)

;;; plantuml-mode-test.el ends here

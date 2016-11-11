;;; plantuml-mode-preview-test.el --- PlantUML Mode preview tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(ert-deftest preview-test ()
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (setq-local plantuml-output-type "utxt")
  (plantuml-preview-string 42 (read-test-file "a-b.puml"))
  (sleep-for 2)
  (should (equal (read-test-file "a-b.txt") (read-preview-buffer))))

(provide 'plantuml-mode-preview-test)

;;; plantuml-mode-preview-test.el ends here

;;; plantuml-mode-preview-test.el --- PlantUML Mode preview tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(defun assert-preview (puml output)
  (plantuml-preview-string 42 (read-test-file puml))
  (sleep-for 2)
  (should (equal (read-test-file output) (read-preview-buffer))))

(ert-deftest preview-test ()
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (setq-local plantuml-output-type "utxt")
  (assert-preview "a-b.puml" "a-b.txt"))

(provide 'plantuml-mode-preview-test)

;;; plantuml-mode-preview-test.el ends here

;;; plantuml-mode-preview-test.el --- PlantUML Mode preview tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(defun assert-preview-content (expected)
  "Assert the contents of the preview buffer to be equal to EXPECTED."
  (should (equal (replace-regexp-in-string " " "~" expected)
                 (replace-regexp-in-string " " "~" (read-preview-buffer)))))

(defun assert-preview (puml output &optional format)
  "Run PlantUML on the source PUML and asserts the result to be
equal to OUTPUT. Can choose the output FORMAT (default: utxt)."
  (if format
    (setq plantuml-output-type format)
    (setq plantuml-output-type "utxt"))
  (plantuml-preview-string nil (read-test-file puml))
  (sleep-for 3)
  (assert-preview-content (read-test-file output)))

(defmacro test-and-cleanup-preview (&rest forms)
  "Run the test described in FORMS and eventually cleanup the preview buffer."
  `(unwind-protect
       (progn
         (setq-local plantuml-jar-path plantuml-test-jar-path)
         ,@forms)
     (cleanup-preview)))

(ert-deftest preview-utxt-test ()
  (test-and-cleanup-preview
   (assert-preview "a-b.puml" "a-b.txt")
   (assert-preview "unicode.puml" "unicode.txt")))

(provide 'plantuml-mode-preview-test)

;;; plantuml-mode-preview-test.el ends here

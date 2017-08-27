;;; plantuml-mode-preview-test.el --- PlantUML Mode preview tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(defun assert-preview (puml output &optional format)
  "Run PlantUML on the source PUML and asserts the result to be
equal to OUTPUT. Can choose the output FORMAT (default: utxt)."
  (if format
    (setq plantuml-output-type format)
    (setq plantuml-output-type "utxt"))
  (plantuml-preview-string 42 (read-test-file puml))
  (sleep-for 3)
  (should (equal (replace-regexp-in-string " " "~" (read-test-file output))
                 (replace-regexp-in-string " " "~" (read-preview-buffer)))))

(defmacro test-and-cleanup-preview (&rest forms)
  "Run the test described in FORMS and eventually cleanup the preview buffer."
  `(unwind-protect
       (progn
         ,@forms)
     (cleanup-preview)))

(ert-deftest preview-utxt-test ()
  (test-and-cleanup-preview
   (setq-local plantuml-jar-path plantuml-test-jar-path)
   (assert-preview "a-b.puml" "a-b.txt")))

(ert-deftest preview-unicode-test ()
  (test-and-cleanup-preview
   (setq-local plantuml-jar-path plantuml-test-jar-path)
   (setq-local plantuml-output-type "utxt")
   (assert-preview "unicode.puml" "unicode.txt")))

(provide 'plantuml-mode-preview-test)

;;; plantuml-mode-preview-test.el ends here

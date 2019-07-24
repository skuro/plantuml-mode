;;; plantuml-config-test.el --- tests for plantuml-mode configuration knobs  -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla
;; Maintainer: Carlo Sciolla
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test user-accessible configuration knobs

;;; Code:

(require 'plantuml-mode)

(ert-deftest plantuml-config-test/set-exec-mode-happy-path ()
  "Test switching execution modes"
  (let ((orig-mode plantuml-exec-mode))
    ;; happy flows:
    (plantuml-set-exec-mode "server")
    (should (equal 'server plantuml-exec-mode))
    (plantuml-set-exec-mode "jar")
    (should (equal 'jar plantuml-exec-mode))
    (plantuml-set-exec-mode "executable")
    (should (equal 'executable plantuml-exec-mode))

    (setq plantuml-exec-mode orig-mode)))

(ert-deftest plantuml-config-test/set-exec-mode-wrong-mode ()
  "Test setting the exec mode with the wrong text"
  (should-error (plantuml-set-exec-mode "turing-machine")))

(provide 'plantuml-mode-config-test)

;;; plantuml-config-test.el ends here

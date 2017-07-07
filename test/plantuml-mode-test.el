;;; plantuml-mode-test.el --- PlantUML Mode tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(ert-deftest initial-jar-location ()
  (should (equal (expand-file-name "~/plantuml.jar")
                 plantuml-jar-path)))

(ert-deftest can-unload-plantuml ()
  (unload-feature 'plantuml-mode t)
  (should (eq nil (boundp 'plantuml-jar-path)))
  (load-plantuml-mode)
  (should (not (eq nil (boundp 'plantuml-jar-path)))))

(ert-deftest debug-install-issues ()
  (unload-feature 'plantuml-mode t)

  (condition-case nil
      (require 'package)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.milkbox.net/packages/"))
    (package-install "plantuml-mode")
    (unload-feature 'plantuml-mode t))

  (load-plantuml-mode))

(provide 'plantuml-mode-test)

;;; plantuml-mode-test.el ends here

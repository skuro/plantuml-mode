;;; plantuml-mode-custom-jar-location-test.el --- PlantUML Mode JAR location tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(ert-deftest custom-jar-location ()
  (setq-local plantuml-jar-path "~/.plantuml/plantuml.jar")
  (should (equal `("-Djava.awt.headless=true" "-jar"
                   ,(expand-file-name "~/.plantuml/plantuml.jar"))
                 (plantuml-render-command)))

  (setq-local plantuml-jar-path "/path/with spaces/plantuml.jar")
  (should (equal `("-Djava.awt.headless=true" "-jar" "/path/with spaces/plantuml.jar")
                 (plantuml-render-command))))

(provide 'plantuml-mode-custom-jar-location-test)

;;; plantuml-mode-custom-jar-location-test.el ends here

;;; plantuml-mode-custom-jar-location-test.el --- PlantUML Mode JAR location tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;;; Code:

(ert-deftest custom-jar-location ()
  (setq-local plantuml-jar-path "~/.plantuml/plantuml.jar")
  (should (equal `("-Djava.awt.headless=true" "-jar" "--illegal-access=deny"
                   ,(expand-file-name "~/.plantuml/plantuml.jar")
                   "-charset" "UTF-8")
                 (plantuml-jar-render-command)))

  (setq-local plantuml-jar-path "/path/with spaces/plantuml.jar")
  (should (equal `("-Djava.awt.headless=true" "-jar" "--illegal-access=deny" "/path/with spaces/plantuml.jar" "-charset" "UTF-8")
                 (plantuml-jar-render-command))))

(provide 'plantuml-mode-custom-jar-location-test)

;;; plantuml-mode-custom-jar-location-test.el ends here

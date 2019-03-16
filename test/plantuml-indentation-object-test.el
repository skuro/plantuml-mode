;;; plantuml-indentation-object-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for object diagrams.

;;; Code:

(ert-deftest plantuml-test-indentation/object-diagram ()
  "Test correct indentation of plantuml object diagram elements.
These code examples are taken from www.plantuml.com
Note: object diagrams use many elements defined for class diagrams."
  (plantuml-test-indent-block

   "
object user {
name = \"Dummy\"
id = 123
}
"
   "
object user {
  name = \"Dummy\"
  id = 123
}
"))

(provide 'plantuml-indentation-object-test)

;;; plantuml-indentation-object-test.el ends here

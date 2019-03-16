;;; plantuml-indentation-ie-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: Carlo Sciolla (skuro)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation of Information Engineering (IE) notation.
;; See https://github.com/plantuml/plantuml/pull/31

;;; Code:

(ert-deftest plantuml-test-block-indentation/ie-entity ()
  "Test correct indentation of an entity block."
  (plantuml-test-indent-block
   "
entity Entity {
* identifying_attribute
--
* mandatory_attribute
optional_attribute
}
"
   "
entity Entity {
  * identifying_attribute
  --
  * mandatory_attribute
  optional_attribute
}
"
   ))

(ert-deftest plantuml-test-indendation/ie-arrows ()
  "Test correct indentation of IE-style arrows."
  (plantuml-test-indent-block
   "
foo1 --{ bar1
foo1 --{ bar2
foo1 --{ bar3

aa --o{ bb
aa --o{ cc
aa --o{ dd
"
   "
foo1 --{ bar1
foo1 --{ bar2
foo1 --{ bar3

aa --o{ bb
aa --o{ cc
aa --o{ dd
"))


;;; plantuml-indentation-ie-test.el ends here

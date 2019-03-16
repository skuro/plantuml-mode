;;; plantuml-indentation-basics-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: Raymond Huang (rymndhng)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test basics like single correct indentation depth etc. Focus is not on diagram contents
;; and context; such things are tested in the diagram specific indentation tests.
;; Test setup is inspired/taken from clojure-mode-indentation-tests

;;; Code:

(defun plantuml-test-add-text-and-position-cursor (txt)
  "Test helper for `plantuml-mode' tests.
Add TXT into the buffer, move cursor to the position of the marker
?| and delete the marker."
  (insert txt)
  (goto-char (point-min))
  (search-forward "|")
  (delete-char -1))

(defun plantuml-test-assert-block-depth (expected txt)
  "Test helper for `plantuml-mode' tests.
Assert the EXPECTED indentation level for the given TXT."
  (with-temp-buffer
    (plantuml-test-add-text-and-position-cursor txt)
    (let ((actual (plantuml-current-block-depth)))
      (should (equal expected actual)))))

(ert-deftest plantuml-test-current-block-depth_bob ()
  "Test `plantuml-current-block-depth' level 0 at beginning of buffer."
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (plantuml-init-once)

  (plantuml-test-assert-block-depth 0 "|
activate p1
  activate p2
    foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_0 ()
  "Test `plantuml-current-block-depth' level 0 at beginning of first line."
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (plantuml-init-once)

  (plantuml-test-assert-block-depth 0 "
|activate p1
  activate p2
    foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_1 ()
  "Test `plantuml-current-block-depth' level 0 at middle of first line."
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (plantuml-init-once)

  (plantuml-test-assert-block-depth 0 "
acti|vate p1
  activate p2
    foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_2 ()
  "Test `plantuml-current-block-depth' level 0 at end of first line"
  (setq-local plantuml-jar-path plantuml-test-jar-path)
  (plantuml-init-once)

  (plantuml-test-assert-block-depth 0 "
activate p1|
  activate p2
    foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_3 ()
  "Test `plantuml-current-block-depth' level 1 at beginning of 2nd line."

  (plantuml-test-assert-block-depth 1 "
activate p1
  |activate p2
    foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_4 ()
  "Test `plantuml-current-block-depth' level 2 at beginning of 3rd line."

  (plantuml-test-assert-block-depth 2 "
activate p1
  activate p2
    |foo
  deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_5 ()
  "Test `plantuml-current-block-depth' level 1 at beginning of 4th line."

  (plantuml-test-assert-block-depth 1 "
activate p1
  activate p2
    foo
  |deactivate p2
deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_6 ()
  "Test `plantuml-current-block-depth' level 0 at beginning of 5th line."

  (plantuml-test-assert-block-depth 0 "
activate p1
  activate p2
    foo
  deactivate p2
|deactivate p1
"))

(ert-deftest plantuml-test-current-block-depth_eob ()
  "Test `plantuml-current-block-depth' level 0 at end of buffer."

  (plantuml-test-assert-block-depth 0 "
activate p1
  activate p2
    foo
  deactivate p2
deactivate p1
|"))

(defun plantuml-test-indent-line (before after)
  "The common code for the line indentation tests.

BEFORE is the text to be inserted into a temporary buffer.
AFTER is the expected text after indentation.

Both, BEFORE and AFTER need to specify point with char |. The
temporary buffer will be put into `plantuml-mode', the char |
representing point will be removed from text. The line with the
removed | will be indented (just this line!) with two spaces for each
level of indentation.

Finally,
1) the indented line will be compared with the same line in AFTER
2) the position of point in the indented line will be compared with
the position of | in AFTER."

  (let* ((expected-cursor-pos (1+ (s-index-of "|" after)))
         (expected-state (delete ?| after)))
    (with-temp-buffer
      ;; fix the JAR location prior to mode initialization
      ;; for some reason, plantuml-mode disregards the setq-local
      (setq-local plantuml-jar-path plantuml-test-jar-path)
      (plantuml-init-once)

      (plantuml-test-add-text-and-position-cursor before)
      (plantuml-mode)

      ;; use 2 spaces instead of one tab for indentation
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 2)
      (indent-according-to-mode)

      (should (equal expected-state (buffer-string)))
      (should (equal expected-cursor-pos (point))))))

(ert-deftest plantuml-test-line-indentation/empty-line-l0 ()
  "Test correct indentation of empty line - indentation level 0."
  (plantuml-test-indent-line "|" "|"))

(ert-deftest plantuml-test-line-indentation/bol-notindent-l0 ()
  "Test correct indentation of a not indented line with point at beginning of line - indentation level 0."
  (plantuml-test-indent-line "|participant A"
                             "|participant A"))

(ert-deftest plantuml-test-line-indentation/mol-notindent-l0 ()
  "Test correct indentation of a not indented line with point at middle of line - indentation level 0."
  (plantuml-test-indent-line "parti|cipant"
                             "parti|cipant"))

(ert-deftest plantuml-test-line-indentation/eol-notindent-l0 ()
  "Test correct indentation of a not indented line with point at end of line - indentation level 0."
  (plantuml-test-indent-line "participant A|"
                             "participant A|"))

(ert-deftest plantuml-test-line-indentation/bol-indented-l0 ()
  "Test correct indentation of an indented line with point at beginning of line - indentation level 0."
  (plantuml-test-indent-line "          |participant A"
                             "|participant A"))

(ert-deftest plantuml-test-line-indentation/mol-indented-l0 ()
  "Test correct indentation of an indented line with point at middle of line - indentation level 0."
  (plantuml-test-indent-line "          parti|cipant"
                             "parti|cipant"))

(ert-deftest plantuml-test-line-indentation/eol-indented-l0 ()
  "Test correct indentation of an indented line with point at end of line - indentation level 0."
  (plantuml-test-indent-line "          participant A|"
                             "participant A|"))

(ert-deftest plantuml-test-line-indentation/empty-line-l1 ()
  "Test correct indentation of empty line - indentation level 1."
  (plantuml-test-indent-line
   "opt A
|"
   "opt A
  |"))

(ert-deftest plantuml-test-line-indentation/bol-notindent-l1 ()
  "Test correct indentation of a not indented line with point at beginning of line - indentation level 1."
  (plantuml-test-indent-line "opt A
|foofoo"
                             "opt A
  |foofoo"))

(ert-deftest plantuml-test-line-indentation/mol-notindent-l1 ()
  "Test correct indentation of a not indented line with point at middle of line - indentation level 1."
  (plantuml-test-indent-line "opt A
foo|foo"
                             "opt A
  foo|foo"))

(ert-deftest plantuml-test-line-indentation/eol-notindent-l1 ()
  "Test correct indentation of a not indented line with point at end of line - indentation level 1."
  (plantuml-test-indent-line "opt A
foofoo|"
                             "opt A
  foofoo|"))

(ert-deftest plantuml-test-line-indentation/bol-indented-l1 ()
  "Test correct indentation of an indented line with point at beginning of line - indentation level 1."
  (plantuml-test-indent-line "            opt A
                              |foofoo"
                             "            opt A
  |foofoo"))

(ert-deftest plantuml-test-line-indentation/mol-indented-l1 ()
  "Test correct indentation of an indented line with point at middle of line - indentation level 1."
  (plantuml-test-indent-line "   opt A
                                        foo|foo"
                             "   opt A
  foo|foo"))

(ert-deftest plantuml-test-line-indentation/eol-indented-l1 ()
  "Test correct indentation of an indented line with point at end of line - indentation level 1."
  (plantuml-test-indent-line "               opt A
                                                          foofoo|"
                             "               opt A
  foofoo|"))



(provide 'plantuml-indentation-basics-test)

;;; plantuml-indentation-basics-test.el ends here

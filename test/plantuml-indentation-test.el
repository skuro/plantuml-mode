;;; plantuml-indentation-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: Raymond Huang (rymndhng)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

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
")
  )

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
")
  )

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
")
  )

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
")
  )

(ert-deftest plantuml-test-current-block-depth_3 ()
  "Test `plantuml-current-block-depth' level 1 at beginning of 2nd line."

  (plantuml-test-assert-block-depth 1 "
activate p1
  |activate p2
    foo
  deactivate p2
deactivate p1
")
  )

(ert-deftest plantuml-test-current-block-depth_4 ()
  "Test `plantuml-current-block-depth' level 2 at beginning of 3rd line."

  (plantuml-test-assert-block-depth 2 "
activate p1
  activate p2
    |foo
  deactivate p2
deactivate p1
")
  )

(ert-deftest plantuml-test-current-block-depth_5 ()
  "Test `plantuml-current-block-depth' level 1 at beginning of 4th line."

  (plantuml-test-assert-block-depth 1 "
activate p1
  activate p2
    foo
  |deactivate p2
deactivate p1
")
  )

(ert-deftest plantuml-test-current-block-depth_6 ()
  "Test `plantuml-current-block-depth' level 0 at beginning of 5th line."

  (plantuml-test-assert-block-depth 0 "
activate p1
  activate p2
    foo
  deactivate p2
|deactivate p1
")
  )

(ert-deftest plantuml-test-current-block-depth_eob ()
  "Test `plantuml-current-block-depth' level 0 at end of buffer."

  (plantuml-test-assert-block-depth 0 "
activate p1
  activate p2
    foo
  deactivate p2
deactivate p1
|")
  )

;; This is taken from https://github.com/clojure-emacs/clojure-mode/blob/master/test/clojure-mode-indentation-test.el
(defmacro plantuml-test-line-indentation (description before after &optional var-bindings)
  "Declare an ert test for line indentation behaviour.
The test will check that the swift indentation command changes the buffer
from one state to another.  It will also test that point is moved to an
expected position.
DESCRIPTION is a symbol describing the test.
BEFORE is the buffer string before indenting, where a pipe (|) represents
point.
AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.
VAR-BINDINGS is an optional let-bindings list.  It can be used to set the
values of customisable variables."

  (declare (indent 1))
  (let ((fname (intern (format "plantuml-test-line-indentation/%s" description))))
    `(ert-deftest ,fname ()
       (let* ((after ,after)
              (expected-cursor-pos (1+ (s-index-of "|" after)))
              (expected-state (delete ?| after))
              ,@var-bindings)
         (with-temp-buffer
           ;; fix the JAR location prior to mode initialization
           ;; for some reason, plantuml-mode disregards the setq-local
           (setq-local plantuml-jar-path plantuml-test-jar-path)
           (plantuml-init-once)

           (plantuml-test-add-text-and-position-cursor ,before)
           (plantuml-mode)

           ;; use 2 spaces instead of one tab for indentation
           (setq-local indent-tabs-mode nil)
           (setq-local tab-width 2)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point))))))))

(plantuml-test-line-indentation toplevel-relationship
  "|Nobody -> [APIGateway]"
  "|Nobody -> [APIGateway]")

(plantuml-test-line-indentation package-block
  "package APackage {
|A -> B
}"
  "package APackage {
  |A -> B
}")

(plantuml-test-line-indentation nested-package
  "package APackage {
|package AnotherPackage {
}
}"
  "package APackage {
  |package AnotherPackage {
}
}")

(plantuml-test-line-indentation empty-package
  "|package Foo {}"
  "|package Foo {}")

(plantuml-test-line-indentation relative-indent
  "package APackage {
database Foo
|A --> B
}
}"
  "package APackage {
database Foo
  |A --> B
}
}"
  )

(plantuml-test-line-indentation note-as
  "note as N1
|This is a note
end note"
  "note as N1
  |This is a note
end note"
  )

(plantuml-test-line-indentation note-of
  "note right of Foo
|This is a note
end note"
  "note right of Foo
  |This is a note
end note"
  )

(plantuml-test-line-indentation alt
                                      "alt choice 1
|A -> B
end
"
                                      "alt choice 1
  |A -> B
end
")

(plantuml-test-line-indentation alt-end
                                "alt choice 1
  A -> B
|end
"
                                "alt choice 1
  A -> B
|end
")

(plantuml-test-line-indentation alt-else
                                "alt choice 1
|else
end
"
                                "alt choice 1
|else
end
")

(plantuml-test-line-indentation alt-else-body
                                "alt choice 1
else
|A -> B
end
"
                                "alt choice 1
else
  |A -> B
end
")

(plantuml-test-line-indentation alt-else-end
                                "alt choice 1
else
|end
"
                                "alt choice 1
else
|end
")

(plantuml-test-line-indentation opt-body
                                "opt have fun
|some text
end"
                                "opt have fun
  |some text
end")

(plantuml-test-line-indentation opt-end
                                "opt have fun
  some text
|end"
                                "opt have fun
  some text
|end")

(plantuml-test-line-indentation activate-activate
                                "
|activate participant_1
"
                                "
|activate participant_1
")

(plantuml-test-line-indentation activate-body
                                "
activate participant_1
|participant_1 -> participant_2 : f()
"

                                "
activate participant_1
  |participant_1 -> participant_2 : f()
")

(plantuml-test-line-indentation activate-deactivate
                                "
activate participant_1
  participant_1 -> participant_2 : f()
|deactivate participant_1"

                                "
activate participant_1
  participant_1 -> participant_2 : f()
|deactivate participant_1")

(plantuml-test-line-indentation activate-deactivate-2
                                "
               activate participant_1
participant_1 -> participant_2 : f()
            |deactivate participant_1"
                                "
               activate participant_1
participant_1 -> participant_2 : f()
|deactivate participant_1")

(plantuml-test-line-indentation activate-deactivate-3
                                "
               activate participant_1
participant_1 -> participant_2 : f()
            deactivate| participant_1"
                                "
               activate participant_1
participant_1 -> participant_2 : f()
deactivate| participant_1")

(defun plantuml-test-indent-block (textblock)
  "Test helper for `plantuml-mode' indentation tests.
TEXTBLOCK will be inserted into a new temporary plantuml buffer. The
whole text will be indented according to the mode. Then, the buffer
contents is returned as a string."

  (with-temp-buffer
    ;; fix the JAR location prior to mode initialization
    ;; for some reason, plantuml-mode disregards the setq-local
    (setq-local plantuml-jar-path plantuml-test-jar-path)
    (plantuml-init-once)

    (insert textblock)
    (goto-char (point-min))
    (plantuml-mode)
    ;; use 2 spaces instead of one tab for indentation
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)

    (indent-region (point-min) (point-max))
    (buffer-string)
    ))

(ert-deftest plantuml-test-block-indentation/activate-deactivate-unindented ()
  "test"
  (should (equal (plantuml-test-indent-block   "
activate participant_1
participant_1 -> participant_2 : f()
deactivate participant_1")
                 "
activate participant_1
  participant_1 -> participant_2 : f()
deactivate participant_1")))

(ert-deftest plantuml-test-block-indentation/activate-deactivate-malformed-indent ()
  "test"
  (should (equal (plantuml-test-indent-block   "
               activate participant_1
participant_1 -> participant_2 : f()
            deactivate participant_1")
                 "
activate participant_1
  participant_1 -> participant_2 : f()
deactivate participant_1")))

(provide 'plantuml-indentation-test)

;;; plantuml-indentation-test.el ends here

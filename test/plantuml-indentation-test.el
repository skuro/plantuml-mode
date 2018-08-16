;;; plantuml-mode-indentation-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: Raymond Huang (rymndhng)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test setup is inspired/taken from clojure-mode-indentation-tests

;;; Code:

;; This is taken from https://github.com/clojure-emacs/clojure-mode/blob/master/test/clojure-mode-indentation-test.el
(defmacro check-indentation (description before after &optional var-bindings)
  "Declare an ert test for indentation behaviour.
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
  (let ((fname (intern (format "indentation/%s" description))))
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

           (insert ,before)
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)
           (plantuml-mode)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point))))))))

(check-indentation toplevel-relationship
  "|Nobody -> [APIGateway]"
  "|Nobody -> [APIGateway]")

(check-indentation package-block
                   "package APackage {
|A -> B
}" "package APackage {
  |A -> B
}")

(check-indentation nested-package
  "package APackage {
|package AnotherPackage {
}
}"
  "package APackage {
  |package AnotherPackage {
}
}")

(check-indentation empty-package
  "|package Foo {}"
  "|package Foo {}")

(check-indentation relative-indent
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

(check-indentation note-as
  "note as N1
|This is a note
end note"
  "note as N1
  |This is a note
end note"
  )

(check-indentation note-of
  "note right of Foo
|This is a note
end note"
  "note right of Foo
  |This is a note
end note"
  )

(check-indentation alt
  "alt choice 1
|A -> B
end
"
  "alt choice 1
  |A -> B
end
")

(check-indentation alt-end
  "alt choice 1
  A -> B
|end
"
  "alt choice 1
  A -> B
|end
")

(check-indentation alt-else
  "alt choice 1
|else
end
"
  "alt choice 1
|else
end
")

(check-indentation alt-else-body
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

(check-indentation alt-else-end
  "alt choice 1
else
|end
"
  "alt choice 1
else
|end
")

(check-indentation alt-else-body
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

(provide 'plantuml-indentation-test)

;;; plantuml-mode-preview-test.el ends here

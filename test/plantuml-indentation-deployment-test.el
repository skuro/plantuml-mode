;;; plantuml-indentation-deployment-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for class diagrams.
;; Most plantuml code examples are taken from www.plantuml.com

;;; Code:

(ert-deftest plantuml-test-indentation/deployment-diagram ()
  "Test correct indentation of plantuml deployment diagram elements."
  (plantuml-test-indent-block

   "
    artifact Foo1 {
    folder Foo2
    }

    folder Foo3 {
    artifact Foo4
    }

    frame Foo5 {
    database Foo6
    }

    cloud vpc {
    node ec2 {
    stack stack
    }
    }

    node Foo1 {
    cloud Foo2
    }

    cloud Foo3 {
    frame Foo4
    }

    database Foo5  {
    storage Foo6
    }

    storage Foo7 {
    storage Foo8
    }

skinparam rectangle {
roundCorner<<Concept>> 25
}

rectangle \"Concept Model\" <<Concept>> {
rectangle \"Example 1\" <<Concept>> as ex1
rectangle \"Another rectangle\"
}
"
   "
artifact Foo1 {
  folder Foo2
}

folder Foo3 {
  artifact Foo4
}

frame Foo5 {
  database Foo6
}

cloud vpc {
  node ec2 {
    stack stack
  }
}

node Foo1 {
  cloud Foo2
}

cloud Foo3 {
  frame Foo4
}

database Foo5  {
  storage Foo6
}

storage Foo7 {
  storage Foo8
}

skinparam rectangle {
  roundCorner<<Concept>> 25
}

rectangle \"Concept Model\" <<Concept>> {
  rectangle \"Example 1\" <<Concept>> as ex1
  rectangle \"Another rectangle\"
}
"))

(provide 'plantuml-indentation-deployment-test)

;;; plantuml-indentation-deployment-test.el ends here

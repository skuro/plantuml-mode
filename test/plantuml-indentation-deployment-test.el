;;; plantuml-indentation-deployment-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for deployment diagrams.
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
"))

(ert-deftest plantuml-test-block-indentation/package-database-nested ()
  "Test correct indentation of two nested blocks, a package and a database.
Note: package is used in class and object diagrams as well, see there for more tests."
  (plantuml-test-indent-block
   "
package APackage {
  database ADatabase {
    A -> B
  }
}
"
   "
package APackage {
  database ADatabase {
    A -> B
  }
}
"))


(provide 'plantuml-indentation-deployment-test)

;;; plantuml-indentation-deployment-test.el ends here

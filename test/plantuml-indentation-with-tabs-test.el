;;; plantuml-indentation-with-tabs-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer, Tobias Marczewski (mtoboid)
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for class diagrams, specifically using tabs.

;;; Code:

(ert-deftest plantuml-test-indentation/tabs/nested-modules ()
  "Test correct indentation of plantuml class diagram elements.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block-with-tabs

   "
@startuml

'some comment
package org.example.module1 {
interface A {
doStuff(): String
getList(): List<Integer>
}

class B {
-name: String
+getName(): String
}
}

package org.example.module2 {
class C {
-count: int
+getCount(): int
}
}

A <|.. B
A <|.. C

@enduml
"
   "
@startuml

'some comment
package org.example.module1 {
	interface A {
		doStuff(): String
		getList(): List<Integer>
	}

	class B {
		-name: String
		+getName(): String
	}
}

package org.example.module2 {
	class C {
		-count: int
		+getCount(): int
	}
}

A <|.. B
A <|.. C

@enduml
"))


(ert-deftest plantuml-test-block-indentation/tabs/package-empty ()
  "Test correct indentation of an empty package block."
  (plantuml-test-indent-block-with-tabs
   "
package APackage ()
interface Inter
"
   "
package APackage ()
interface Inter
"))


(ert-deftest platuml-test-block-indentation/tabs/package-interface-nested ()
  "Test correct indentation of two nested blocks, a package and an interface
Note: package is used in deployment and object diagrams as well, see there for more tests."
  (plantuml-test-indent-block-with-tabs
   "
package foo {
interface Bar {
baz
}
}
"
   "
package foo {
	interface Bar {
		baz
	}
}
"))


(provide 'plantuml-indentation-with-tabs-test)

;;; plantuml-indentation-with-tabs-test.el ends here

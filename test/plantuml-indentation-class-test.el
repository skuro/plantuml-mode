;;; plantuml-indentation-class-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for class diagrams.

;;; Code:

(ert-deftest plantuml-test-indentation/class-diagram ()
  "Test correct indentation of plantuml class diagram elements.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block

   "
            class Dummy {
            String data
            void methods()
            }

            class Flight {
            flightNumber : Integer
            departureTime : Date
            }

            class Dummy1 {
            {field} A field (despite parentheses)
            {method} Some method
            }

            class Dummy2 {
            -field1
            #field2
            ~method1()
            +method2()
            }

            class Dummy3 {
            {static} String id
            {abstract} void methods()
            }

            class Foo1 {
            You can use
            several lines
            ..
            as you want
            and group
            ==
            things together.
            __
            You can have as many groups
            as you want
            --
            End of class
            }

            class User {
            .. Simple Getter ..
            + getName()
            + getAddress()
            .. Some setter ..
            + setName()
            __ private data __
            int age
            -- encrypted --
            String password
            }

            class ArrayList {
            Object[] elementData
            size()
            }

            enum TimeUnit {
            DAYS
            HOURS
            MINUTES
            }

            class Dummy4 <<Serializable>> {
            String name
            }

            class Foo<? extends Element> {
            int size()
            }

            abtract class AbstractC {
            int size()
            }

            interface InterfaceC {
            int size()
            }

            package \"Classic Collections\" #DDDDDD {
            Object <|-- ArrayList
            }

            package net.sourceforge.plantuml {
            Object <|-- Demo1
            Demo1 *- Demo2
            }

            package foo1 <<Node>> {
            class Class1
            }

            package foo2 <<Rectangle>> {
            class Class2
            }

            package foo3 <<Folder>> {
            class Class3
            }

            package foo4 <<Frame>> {
            class Class4
            }

            package foo5 <<Cloud>> {
            class Class5
            }

            package foo6 <<Database>> {
            class Class6
            }

            package foo1.foo2 {
            class ObjectFoo1Foo2
            }

            package foo1.foo2.foo3 {
            class Objectfoo1.foo2.foo3
            }

            namespace net.dummy #DDDDDD {
            .BaseClass <|-- Person
            Meeting o-- Person

            .BaseClass <|- Meeting
            }

            namespace net.foo {
            net.dummy.Person  <|- Person
            .BaseClass <|-- Person

            net.dummy.Meeting o-- Person
            }

            set namespaceSeparator ::
            class X1::X2::foo {
            some info
            }

            together {
            class Together1
            class Together2
            class Together3
            }
"
   "
class Dummy {
  String data
  void methods()
}

class Flight {
  flightNumber : Integer
  departureTime : Date
}

class Dummy1 {
  {field} A field (despite parentheses)
  {method} Some method
}

class Dummy2 {
  -field1
  #field2
  ~method1()
  +method2()
}

class Dummy3 {
  {static} String id
  {abstract} void methods()
}

class Foo1 {
  You can use
  several lines
  ..
  as you want
  and group
  ==
  things together.
  __
  You can have as many groups
  as you want
  --
  End of class
}

class User {
  .. Simple Getter ..
  + getName()
  + getAddress()
  .. Some setter ..
  + setName()
  __ private data __
  int age
  -- encrypted --
  String password
}

class ArrayList {
  Object[] elementData
  size()
}

enum TimeUnit {
  DAYS
  HOURS
  MINUTES
}

class Dummy4 <<Serializable>> {
  String name
}

class Foo<? extends Element> {
  int size()
}

abtract class AbstractC {
  int size()
}

interface InterfaceC {
  int size()
}

package \"Classic Collections\" #DDDDDD {
  Object <|-- ArrayList
}

package net.sourceforge.plantuml {
  Object <|-- Demo1
  Demo1 *- Demo2
}

package foo1 <<Node>> {
  class Class1
}

package foo2 <<Rectangle>> {
  class Class2
}

package foo3 <<Folder>> {
  class Class3
}

package foo4 <<Frame>> {
  class Class4
}

package foo5 <<Cloud>> {
  class Class5
}

package foo6 <<Database>> {
  class Class6
}

package foo1.foo2 {
  class ObjectFoo1Foo2
}

package foo1.foo2.foo3 {
  class Objectfoo1.foo2.foo3
}

namespace net.dummy #DDDDDD {
  .BaseClass <|-- Person
  Meeting o-- Person

  .BaseClass <|- Meeting
}

namespace net.foo {
  net.dummy.Person  <|- Person
  .BaseClass <|-- Person

  net.dummy.Meeting o-- Person
}

set namespaceSeparator ::
class X1::X2::foo {
  some info
}

together {
  class Together1
  class Together2
  class Together3
}
"))


(ert-deftest plantuml-test-block-indentation/class/package-empty ()
  "Test correct indentation of an empty package block."
  (plantuml-test-indent-block
   "
package APackage ()
interface Inter
"
   "
package APackage ()
interface Inter
"))


(ert-deftest platuml-test-block-indentation/class/package-interface-nested ()
  "Test correct indentation of two nested blocks, a package and an interface
Note: package is used in deployment and object diagrams as well, see there for more tests."
  (plantuml-test-indent-block
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


(provide 'plantuml-indentation-class-test)

;;; plantuml-indentation-class-test.el ends here

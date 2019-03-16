;;; plantuml-indentation-commons-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation of structures and elements that are common
;; to many/all diagrams.

;;; Code:

(ert-deftest plantuml-test-indentation/commons/skinparam ()
  "Test correct indentation of skinparam elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block

   "
skinparam roundcorner 20

skinparam rectangle {
roundCorner<<Concept>> 25
}
"
   "
skinparam roundcorner 20

skinparam rectangle {
  roundCorner<<Concept>> 25
}
"))

(ert-deftest plantuml-test-indentation/commons/rectangle ()
  "Test correct indentation of rectangle elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block

   "
rectangle \"Concept Model\" <<Concept>> {
rectangle \"Example 1\" <<Concept>> as ex1
rectangle \"Another rectangle\"
}
"
   "
rectangle \"Concept Model\" <<Concept>> {
  rectangle \"Example 1\" <<Concept>> as ex1
  rectangle \"Another rectangle\"
}
"))

(ert-deftest plantuml-test-indentation/commons/title ()
  "Test correct indentation of title elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block
   "
title
<font color=red>Warning:</font>
Do not use in production.
end title
"
   "
title
  <font color=red>Warning:</font>
  Do not use in production.
end title
"   ))

(ert-deftest plantuml-test-indentation/commons/header ()
  "Test correct indentation of header elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block
   "
header
this is a header
endheader

center header
this is a centered header
endheader

right header
this is a header on right
endheader

left header
this is a header on left
endheader

center left header
this is no correct header
endheader

header left
this is no correct a header
endheader
"
   "
header
  this is a header
endheader

center header
  this is a centered header
endheader

right header
  this is a header on right
endheader

left header
  this is a header on left
endheader

center left header
this is no correct header
endheader

header left
this is no correct a header
endheader
"   ))

(ert-deftest plantuml-test-indentation/commons/footer ()
  "Test correct indentation of footer elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block
   "
footer
this is a footer
endfooter

center footer
this is a centered footer
endfooter

right footer
this is a footer on right
endfooter

left footer
this is a footer on left
endfooter

center left footer
this is no correct footer
endfooter

footer left
this is no correct a footer
endfooter
"
   "
footer
  this is a footer
endfooter

center footer
  this is a centered footer
endfooter

right footer
  this is a footer on right
endfooter

left footer
  this is a footer on left
endfooter

center left footer
this is no correct footer
endfooter

footer left
this is no correct a footer
endfooter
"   ))


(ert-deftest plantuml-test-indentation/commons/legend ()
  "Test correct indentation of legend elements, which are common to many plantuml diagrams:"
  (plantuml-test-indent-block
   "
legend
Short legend
endlegend

legend bottom
bottom legend
endlegend

legend top
top legend
endlegend

legend center
centered legend
endlegend

legend right
legend on right
endlegend

legend left
legend on left
endlegend

legend bottom left
legend on bottom left
endlegend

legend top left
legend on top left
endlegend

legend bottom right
legend on bottom right
endlegend

legend top right
legend on top right
endlegend
"
   "
legend
  Short legend
endlegend

legend bottom
  bottom legend
endlegend

legend top
  top legend
endlegend

legend center
  centered legend
endlegend

legend right
  legend on right
endlegend

legend left
  legend on left
endlegend

legend bottom left
  legend on bottom left
endlegend

legend top left
  legend on top left
endlegend

legend bottom right
  legend on bottom right
endlegend

legend top right
  legend on top right
endlegend
"   ))


(ert-deftest plantuml-test-indentation/commons/legend-noindent ()
  "Test the not-indentation of false legend elements."
  (plantuml-test-indent-block
   "
legend bottom top
this is no correct legend
endlegend

legend right bottom
this is no correct legend
endlegend

legend left top
this is no correct legend
endlegend

legend center right
this is no correct legend
endlegend

legend center left
this is no correct legend
endlegend

legend right left
this is no correct legend
endlegend
"
   "
legend bottom top
this is no correct legend
endlegend

legend right bottom
this is no correct legend
endlegend

legend left top
this is no correct legend
endlegend

legend center right
this is no correct legend
endlegend

legend center left
this is no correct legend
endlegend

legend right left
this is no correct legend
endlegend
"
   ))

(ert-deftest plantuml-test-indentation/commons/multiline-macro ()
  "Test the indentation of multiline macro elements."
  (plantuml-test-indent-block

   "
!define DOUBLE(x) x x
!definelong AUTHEN(x,y)
x -> y : DOUBLE(hello)
y -> x : ok
!enddefinelong

AUTHEN(Bob,Alice)
"
   "
!define DOUBLE(x) x x
!definelong AUTHEN(x,y)
  x -> y : DOUBLE(hello)
  y -> x : ok
!enddefinelong

AUTHEN(Bob,Alice)
"
   ))
;;; plantuml-indentation-commons-test.el ends here

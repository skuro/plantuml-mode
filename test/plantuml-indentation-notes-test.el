;;; plantuml-indentation-notes-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation of note elements.
;; Notes are used in almost all diagrams, but not all types of notes
;; are supported in every diagram type.

;;; Code:

(ert-deftest plantuml-test-block-indentation/note-simple ()
  "Test correct indentation of a simple note block."
  (plantuml-test-indent-block
   "
note right: single line note

note right
multi line note
end note
"
   "
note right: single line note

note right
  multi line note
end note
"
   ))

(ert-deftest plantuml-test-block-indentation/note-left-right ()
  "Test correct indentation of a note left/right block."
  (plantuml-test-indent-block
   "
(*) --> \"Some Activity\"
note right: This activity has to be defined
\"Some Activity\" --> (*)

note right
This note is on
several lines
end note

(*) --> \"Some Activity\"
note left: This activity has to be defined
\"Some Activity\" --> (*)

note left
This note is on
several lines
end note
"
   "
(*) --> \"Some Activity\"
note right: This activity has to be defined
\"Some Activity\" --> (*)

note right
  This note is on
  several lines
end note

(*) --> \"Some Activity\"
note left: This activity has to be defined
\"Some Activity\" --> (*)

note left
  This note is on
  several lines
end note
"   ))

(ert-deftest plantuml-test-block-indentation/note-xxx-of ()
  "Test correct indentation of a note xxx of block.
plantuml.com:
You can use the note left of, note right of, note top of,
note bottom of keywords to define notes related to a single
object."
  (plantuml-test-indent-block
   "
note right of Alice: This is displayed right of Alice.

note left of Alice #aqua
This is displayed
left of Alice.
end note

note left of Alice: This is displayed left of Alice.

note right of Alice #aqua
This is displayed
right of Alice.
end note

note right of (Use)
A note can also
be on several lines
end note

note left of HTTP : Web Service only

note right of [First Component]
A note can also
be on several lines
end note
"
   "
note right of Alice: This is displayed right of Alice.

note left of Alice #aqua
  This is displayed
  left of Alice.
end note

note left of Alice: This is displayed left of Alice.

note right of Alice #aqua
  This is displayed
  right of Alice.
end note

note right of (Use)
  A note can also
  be on several lines
end note

note left of HTTP : Web Service only

note right of [First Component]
  A note can also
  be on several lines
end note
"
   ))

(ert-deftest plantuml-test-block-indentation/note-on-link ()
  "Test correct indentation of a note xxx of block.
plantuml.com:
“You can also use note left on link, note right on link,
note top on link, note bottom on link if you want to change
the relative position of the note with the label.”"
  (plantuml-test-indent-block
   "
class Dummy
Dummy --> Foo : A link
note on link #red: note that is red

Dummy --> Foo2 : Another link
note right on link #blue
this is my note on right link
and in blue
end note

note left on link #00FFFF
this is my note on left link
and in color as number
end note
"
   "
class Dummy
Dummy --> Foo : A link
note on link #red: note that is red

Dummy --> Foo2 : Another link
note right on link #blue
  this is my note on right link
  and in blue
end note

note left on link #00FFFF
  this is my note on left link
  and in color as number
end note
"
   ))

(ert-deftest plantuml-test-block-indentation/note-over ()
  "Test correct indentation of a note-over block."
  (plantuml-test-indent-block
   "
note over Alice: This is displayed over Alice.

note over Alice, Bob #FFAAAA: This is displayed over Bob and Alice.

note over Bob, Alice
This is yet another
example of
a long note.
end note
"
   "
note over Alice: This is displayed over Alice.

note over Alice, Bob #FFAAAA: This is displayed over Bob and Alice.

note over Bob, Alice
  This is yet another
  example of
  a long note.
end note
"
   ))

;; Here we have an inconsistency (again) in plantuml syntax
;; single line ‘note as …’ does not contain a ?:
;;
;; (ert-deftest plantuml-test-block-indentation/note-as ()
;;   "Test correct indentation of a note-as block."
;;   (plantuml-test-indent-block
;;    "
;; :Main Admin: as Admin
;; (Use the application) as (Use)

;; User -> (Start)
;; User --> (Use)

;; Admin ---> (Use)

;; note right of Admin : This is an example.

;; note right of (Use)
;; A note can also
;; be on several lines
;; end note

;; note \"This note is connected to several objects.\" as N2 #RED
;; (Start) .. N2
;; N2 .. (Use)

;; note  as N3 #blue
;; This note is connected
;; to several objects as well.
;; end note
;; (Start) .. N3
;; N3 .. Admin
;; "
;;    "
;; :Main Admin: as Admin
;; (Use the application) as (Use)

;; User -> (Start)
;; User --> (Use)

;; Admin ---> (Use)

;; note right of Admin : This is an example.

;; note right of (Use)
;;   A note can also
;;   be on several lines
;; end note

;; note \"This note is connected to several objects.\" as N2 #RED
;; (Start) .. N2
;; N2 .. (Use)

;; note  as N3 #blue
;;   This note is connected
;;   to several objects as well.
;; end note

;; (Start) .. N3
;; N3 .. Admin
;; "
;;    ))

(ert-deftest plantuml-test-block-indentation/note-floating ()
  "Test correct indentation of a floating note block."
  (plantuml-test-indent-block

   "
floating note left: This is a note
:foo2;

floating note right
This note is on several
//lines// and can
contain <b>HTML</b>
====
* Calling the method \"\"foo()\"\" is prohibited
end note
"
   "
floating note left: This is a note
:foo2;

floating note right
  This note is on several
  //lines// and can
  contain <b>HTML</b>
  ====
  * Calling the method \"\"foo()\"\" is prohibited
end note
"
   ))

(ert-deftest plantuml-test-block-indentation/note-h-r-note ()
  "Test correct indentation of a [hr]note block."
  (plantuml-test-indent-block
   "
caller -> server : conReq
rnote over caller : idle
hnote over caller : idle
caller <- server : conConf

rnote over server
r as rectangle
h as hexagon
endrnote

hnote over server
r as rectangle
h as hexagon
endrnote
"
   "
caller -> server : conReq
rnote over caller : idle
hnote over caller : idle
caller <- server : conConf

rnote over server
  r as rectangle
  h as hexagon
endrnote

hnote over server
  r as rectangle
  h as hexagon
endrnote
"
   ))

(ert-deftest plantuml-test-block-indentation/note-creole-html ()
  "Test correct indentation of a note block with creole/html."
  (plantuml-test-indent-block

   "
participant Alice
participant \"The **Famous** Bob\" as Bob

Alice -> Bob : hello --there--
... Some ~~long delay~~ ...
Bob -> Alice : ok
note left
This is **bold**
This is //italics//
This is \"\"monospaced\"\"
This is --stroked--
This is __underlined__
This is ~~waved~~
end note

Alice -> Bob : A //well formatted// message
note right of Alice
This is <back:cadetblue><size:18>displayed</size></back>
__left of__ Alice.
end note
note left of Bob
<u:red>This</u> is <color #118888>displayed</color>
**<color purple>left of</color> <s:red>Alice</strike> Bob**.
end note
note over Alice, Bob
<w:#FF33FF>This is hosted</w> by <img sourceforge.jpg>
end note

"
   "
participant Alice
participant \"The **Famous** Bob\" as Bob

Alice -> Bob : hello --there--
... Some ~~long delay~~ ...
Bob -> Alice : ok
note left
  This is **bold**
  This is //italics//
  This is \"\"monospaced\"\"
  This is --stroked--
  This is __underlined__
  This is ~~waved~~
end note

Alice -> Bob : A //well formatted// message
note right of Alice
  This is <back:cadetblue><size:18>displayed</size></back>
  __left of__ Alice.
end note
note left of Bob
  <u:red>This</u> is <color #118888>displayed</color>
  **<color purple>left of</color> <s:red>Alice</strike> Bob**.
end note
note over Alice, Bob
  <w:#FF33FF>This is hosted</w> by <img sourceforge.jpg>
end note

"
   ))

;;; plantuml-indentation-notes-test.el ends here

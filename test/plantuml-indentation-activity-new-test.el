;;; plantuml-indentation-activity-old-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for activity (old version) diagrams.

;;; Code:


(ert-deftest plantuml-test-indentation/activity-new/start-stop ()
  "Test correct indentation of plantuml activity-new diagram elements: start-stop.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
"@startuml
start
:Hello world;
:This is defined on
several **lines**;
end
@enduml"

"@startuml
start
:Hello world;
:This is defined on
several **lines**;
end
@enduml"))

(ert-deftest plantuml-test-indentation/activity-new/conditional ()
  "Test correct indentation of plantuml activity-new diagram conditionals.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
"@startuml
start
if (condition A) then (yes)
:Text 1;
elseif (condition B) then (yes)
:Text 2;
stop
elseif (condition C) then (yes)
:Text 3;
elseif (condition D) then (yes)
:Text 4;
else (nothing)
:Text else;
endif
stop
@enduml"

"@startuml
start
if (condition A) then (yes)
  :Text 1;
elseif (condition B) then (yes)
  :Text 2;
  stop
elseif (condition C) then (yes)
  :Text 3;
elseif (condition D) then (yes)
  :Text 4;
else (nothing)
  :Text else;
endif
stop
@enduml"))

(ert-deftest plantuml-test-indentation/activity-new/repeat-loop ()
  "Test correct indentation of plantuml activity-new repeat loop
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
"@startuml

start

repeat
:read data;
:generate diagrams;
repeat while (more data?) is (yes)
->no;
stop

@enduml"

"@startuml

start

repeat
  :read data;
  :generate diagrams;
repeat while (more data?) is (yes)
->no;
stop

@enduml"))


(ert-deftest plantuml-test-indentation/activity-new/while-loop ()
  "Test correct indentation of plantuml activity-new while loop
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
"@startuml

start

while (data available?)
:read data;
:generate diagrams;
endwhile

stop

@enduml"

"@startuml

start

while (data available?)
  :read data;
  :generate diagrams;
endwhile

stop

@enduml")

    (plantuml-test-indent-block
"@startuml
while (check filesize ?) is (not empty)
:read file;
endwhile (empty)
:close file;
@enduml"

"@startuml
while (check filesize ?) is (not empty)
  :read file;
endwhile (empty)
:close file;
@enduml"))

(ert-deftest plantuml-test-indentation/activity-new/fork ()
  "Test correct indentation of plantuml activity-new forks
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
   "@startuml

start

if (multiprocessor?) then (yes)
fork
:Treatment 1;
fork again
:Treatment 2;
end fork
else (monoproc)
:Treatment 1;
:Treatment 2;
endif

@enduml"
   "@startuml

start

if (multiprocessor?) then (yes)
  fork
    :Treatment 1;
  fork again
    :Treatment 2;
  end fork
else (monoproc)
  :Treatment 1;
  :Treatment 2;
endif

@enduml"))

(ert-deftest plantuml-test-indentation/activity-new/notes ()
  "Test correct indentation of plantuml activity-new notes
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
"@startuml

start
:foo1;
floating note left: This is a note
:foo2;
note right
This note is on several
//lines// and can
contain <b>HTML</b>
====
* Calling the method \"foo()\" is prohibited
end note
stop

@enduml"

"@startuml

start
:foo1;
floating note left: This is a note
:foo2;
note right
  This note is on several
  //lines// and can
  contain <b>HTML</b>
  ====
  * Calling the method \"foo()\" is prohibited
end note
stop

@enduml"))





(provide 'plantuml-indentation-activity-new-test)

;;; plantuml-indentation-activity-old-test.el ends here

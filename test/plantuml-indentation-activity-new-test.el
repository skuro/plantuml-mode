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


(provide 'plantuml-indentation-activity-new-test)

;;; plantuml-indentation-activity-old-test.el ends here

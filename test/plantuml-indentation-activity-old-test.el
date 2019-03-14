;;; plantuml-indentation-activity-old-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for activity (old version) diagrams.

;;; Code:


(ert-deftest plantuml-test-indentation/activity-old-diagram/branches ()
  "Test correct indentation of plantuml activity-old diagram elements: branches.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block
   "
if \"Some Test\" then
-->[true] \"Some Activity\"
--> \"Another activity\"
-right-> (*)
else
->[false] \"Something else\"
-->[Ending process] (*)
endif

(*)  --> \"check input\"
If \"input is verbose\" then
--> [Yes] \"turn on verbosity\"
--> \"run command\"
else
--> \"run command\"
Endif
-->(*)
"
   "
if \"Some Test\" then
  -->[true] \"Some Activity\"
  --> \"Another activity\"
  -right-> (*)
else
  ->[false] \"Something else\"
  -->[Ending process] (*)
endif

(*)  --> \"check input\"
If \"input is verbose\" then
  --> [Yes] \"turn on verbosity\"
  --> \"run command\"
else
  --> \"run command\"
Endif
-->(*)
"
   ))

(ert-deftest plantuml-test-indentation/activity-old-diagram/more-on-branches ()
  "Test correct indentation of plantuml activity-old diagram elements: more on branches.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block

   "
(*) --> if \"Some Test\" then

-->[true] \"activity 1\"

if \"\" then
-> \"activity 3\" as a3
else
if \"Other test\" then
-left-> \"activity 5\"
else
--> \"activity 6\"
endif
endif

else

->[false] \"activity 2\"

endif

a3 --> if \"last test\" then
--> \"activity 7\"
else
-> \"activity 8\"
endif
"
   "
(*) --> if \"Some Test\" then

  -->[true] \"activity 1\"

  if \"\" then
    -> \"activity 3\" as a3
  else
    if \"Other test\" then
      -left-> \"activity 5\"
    else
      --> \"activity 6\"
    endif
  endif

else

  ->[false] \"activity 2\"

endif

a3 --> if \"last test\" then
  --> \"activity 7\"
else
  -> \"activity 8\"
endif
"
   ))

(ert-deftest plantuml-test-indentation/activity-old-diagram/partitions ()
  "Test correct indentation of plantuml activity-old diagram elements: partitions.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block

   "
partition Conductor {
(*) --> \"Climbs on Platform\"
--> === S1 ===
--> Bows
}

partition Audience #LightSkyBlue {
=== S1 === --> Applauds
}

partition Conductor {
Bows --> === S2 ===
--> WavesArmes
Applauds --> === S2 ===
}

partition Orchestra #CCCCEE {
WavesArmes --> Introduction
--> \"Play music\"
}
"
"
partition Conductor {
  (*) --> \"Climbs on Platform\"
  --> === S1 ===
  --> Bows
}

partition Audience #LightSkyBlue {
  === S1 === --> Applauds
}

partition Conductor {
  Bows --> === S2 ===
  --> WavesArmes
  Applauds --> === S2 ===
}

partition Orchestra #CCCCEE {
  WavesArmes --> Introduction
  --> \"Play music\"
}
"))

(ert-deftest plantuml-test-indentation/activity-old-diagram/complete-example ()
  "Test correct indentation of plantuml activity-old diagram elements: complete example.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block

   "
title Servlet Container

(*) --> \"ClickServlet.handleRequest()\"
--> \"new Page\"

if \"Page.onSecurityCheck\" then
->[true] \"Page.onInit()\"

if \"isForward?\" then
->[no] \"Process controls\"

if \"continue processing?\" then
-->[yes] ===RENDERING===
else
-->[no] ===REDIRECT_CHECK===
endif

else
-->[yes] ===RENDERING===
endif

if \"is Post?\" then
-->[yes] \"Page.onPost()\"
--> \"Page.onRender()\" as render
--> ===REDIRECT_CHECK===
else
-->[no] \"Page.onGet()\"
--> render
endif

else
-->[false] ===REDIRECT_CHECK===
endif

if \"Do redirect?\" then
->[yes] \"redirect request\"
--> ==BEFORE_DESTROY===
else
if \"Do Forward?\" then
-left->[yes] \"Forward request\"
--> ==BEFORE_DESTROY===
else
-right->[no] \"Render page template\"
--> ==BEFORE_DESTROY===
endif
endif

--> \"Page.onDestroy()\"
-->(*)
"
   "
title Servlet Container

(*) --> \"ClickServlet.handleRequest()\"
--> \"new Page\"

if \"Page.onSecurityCheck\" then
  ->[true] \"Page.onInit()\"

  if \"isForward?\" then
    ->[no] \"Process controls\"

    if \"continue processing?\" then
      -->[yes] ===RENDERING===
    else
      -->[no] ===REDIRECT_CHECK===
    endif

  else
    -->[yes] ===RENDERING===
  endif

  if \"is Post?\" then
    -->[yes] \"Page.onPost()\"
    --> \"Page.onRender()\" as render
    --> ===REDIRECT_CHECK===
  else
    -->[no] \"Page.onGet()\"
    --> render
  endif

else
  -->[false] ===REDIRECT_CHECK===
endif

if \"Do redirect?\" then
  ->[yes] \"redirect request\"
  --> ==BEFORE_DESTROY===
else
  if \"Do Forward?\" then
    -left->[yes] \"Forward request\"
    --> ==BEFORE_DESTROY===
  else
    -right->[no] \"Render page template\"
    --> ==BEFORE_DESTROY===
  endif
endif

--> \"Page.onDestroy()\"
-->(*)
"
   ))


(provide 'plantuml-indentation-activity-old-test)

;;; plantuml-indentation-activity-old-test.el ends here

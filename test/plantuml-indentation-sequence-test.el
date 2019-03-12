;;; plantuml-indentation-sequence-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for sequence diagrams.

;;; Code:


(ert-deftest platuml-test-block-indentation/sequence/box ()
  "Test correct indentation of a box block"
  (plantuml-test-indent-block
   "
box \"Device with USB connector\"
actor Human
participant UsbDetector
end box
"
   "
box \"Device with USB connector\"
  actor Human
  participant UsbDetector
end box
" ))

(ert-deftest platuml-test-block-indentation/sequence/ref ()
  "Test correct indentation of a ref block"
  (plantuml-test-indent-block
   "
participant Alice
actor Bob
participant \"Great Cesar\"

ref over Alice, Bob : init

Alice -> Bob : hello

ref over Bob, \"Great Cesar\"
This can be on
several lines
end ref

ref over Bob
This is a ref over Bob
end ref

ref over \"Great Cesar\"
This is a ref over \"Great Cesar\"
end ref
"

   "
participant Alice
actor Bob
participant \"Great Cesar\"

ref over Alice, Bob : init

Alice -> Bob : hello

ref over Bob, \"Great Cesar\"
  This can be on
  several lines
end ref

ref over Bob
  This is a ref over Bob
end ref

ref over \"Great Cesar\"
  This is a ref over \"Great Cesar\"
end ref
" ))


(ert-deftest plantuml-test-block-indentation/sequence/alt-end ()
  "Test correct indentation of an alt-end block.
The alt-keyword is NOT followed by some text."
  (plantuml-test-indent-block
   "
alt
A -> B
end
"
   "
alt
  A -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/alt-end-with-label ()
  "Test correct indentation of an alt-end block.
The alt-keyword is followed by some text."
  (plantuml-test-indent-block
   "
alt choice 1
A -> B
end
"
   "
alt choice 1
  A -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/alt-else-end ()
  "Test correct indentation of an alt-else-end block."
  (plantuml-test-indent-block
   "
alt choice 1
A -> B
else
B -> C
end
"
   "
alt choice 1
  A -> B
else
  B -> C
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/opt ()
  "Test correct indentation of an opt block.
The opt-keyword is NOT followed by some text."
  (plantuml-test-indent-block
   "
opt
A -> B
end
"
   "
opt
  A -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/opt-with-label ()
  "Test correct indentation of an opt block.
The opt-keyword is followed by some text."
  (plantuml-test-indent-block
   "
opt event triggered
A -> B
end
"
   "
opt event triggered
  A -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/par ()
  "Test correct indentation of a par block.
The par-keyword is NOT followed by some text."
  (plantuml-test-indent-block
   "
par
A -> B
else
C -> B
end
"
   "
par
  A -> B
else
  C -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/par-with-label ()
  "Test correct indentation of a par block.
The par-keyword is followed by some text."
  (plantuml-test-indent-block
   "
par a text label
A -> B
else
C -> B
end
"
   "
par a text label
  A -> B
else
  C -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/group ()
  "Test correct indentation of a group block.
The group-keyword is NOT followed by some text."
  (plantuml-test-indent-block
   "
group
A -> B
else
C -> B
end
"
   "
group
  A -> B
else
  C -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/group-with-label ()
  "Test correct indentation of a group block.
The group-keyword is followed by some text."
  (plantuml-test-indent-block
   "
group my own label
A -> B
else
C -> B
end
"
   "
group my own label
  A -> B
else
  C -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/critical ()
  "Test correct indentation of a critical block.
The critical-keyword is NOT followed by some text."
  (plantuml-test-indent-block
   "
critical
A -> B
else
C -> B
end
"
   "
critical
  A -> B
else
  C -> B
end
" ))

(ert-deftest plantuml-test-block-indentation/sequence/critical-with-label ()
  "Test correct indentation of a critical block.
The critical-keyword is followed by some text."
  (plantuml-test-indent-block
   "
critical my own label
A -> B
else
C -> B
end
"
   "
critical my own label
  A -> B
else
  C -> B
end
" ))


(ert-deftest plantuml-test-block-indentation/sequence/activate-deactivate ()
  "Test correct indentation of an activate-deactivate block."
  (plantuml-test-indent-block
   "
activate participant_1
participant_1 -> participant_2 : f()
deactivate participant_1
"
   "
activate participant_1
  participant_1 -> participant_2 : f()
deactivate participant_1
"))

(ert-deftest plantuml-test-block-indentation/sequence/activate-deactivate-nested ()
  "Test correct indentation of two nested activate-deactivate blocks."
  (plantuml-test-indent-block
   "
activate participant_1
activate participant_2
participant_1 -> participant_2 : f()
deactivate participant_2
deactivate participant_1
"
   "
activate participant_1
  activate participant_2
    participant_1 -> participant_2 : f()
  deactivate participant_2
deactivate participant_1
"))


(ert-deftest plantuml-test-indentation/sequence-diagram ()
  "Test correct indentation of plantuml sequence diagram elements.
These code examples are taken from www.plantuml.com."
  (plantuml-test-indent-block
   "
Alice -> Bob: Authentication Request

alt successful case

Bob -> Alice: Authentication Accepted

else some kind of failure

Bob -> Alice: Authentication Failure
group My own label
Alice -> Log : Log attack start
loop 1000 times
Alice -> Bob: DNS Attack
end
Alice -> Log : Log attack end
end

else Another type of failure

Bob -> Alice: Please repeat

end
"
   "
Alice -> Bob: Authentication Request

alt successful case

  Bob -> Alice: Authentication Accepted

else some kind of failure

  Bob -> Alice: Authentication Failure
  group My own label
    Alice -> Log : Log attack start
    loop 1000 times
      Alice -> Bob: DNS Attack
    end
    Alice -> Log : Log attack end
  end

else Another type of failure

  Bob -> Alice: Please repeat

end
"))


(provide 'plantuml-indentation-sequence-test)

;;; plantuml-indentation-sequence-test.el ends here

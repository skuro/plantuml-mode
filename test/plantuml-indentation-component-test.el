;;; plantuml-indentation-component-test.el --- PlantUML Mode indentation tests   -*- lexical-binding: t; -*-

;; Author: René Schmelzer
;; Maintainer: Carlo Sciolla (skuro)
;; URL: https://github.com/skuro/plantuml-mode

;;; Commentary:

;; Test indentation for component diagrams.

;;; Code:

(ert-deftest plantuml-test-indentation/component-diagram ()
  "Test correct indentation of plantuml component diagram elements.
These code examples are taken from www.plantuml.com"
  (plantuml-test-indent-block

   "
package \"Some Group\" {
HTTP - [First Component]
[Another Component]
}

node \"Other Groups\" {
FTP - [Second Component]
[First Component] --> FTP
}

cloud {
[Example 1]
}

database \"MySql\" {
folder \"This is my folder\" {
[Folder 3]
}
frame \"Foo\" {
[Frame 4]
}
}
"
   "
package \"Some Group\" {
  HTTP - [First Component]
  [Another Component]
}

node \"Other Groups\" {
  FTP - [Second Component]
  [First Component] --> FTP
}

cloud {
  [Example 1]
}

database \"MySql\" {
  folder \"This is my folder\" {
    [Folder 3]
  }
  frame \"Foo\" {
    [Frame 4]
  }
}
"))

(provide 'plantuml-indentation-component-test)

;;; plantuml-indentation-component-test.el ends here

;;; plantuml-server-test.el --- Test against PlantUML servers (regular and pico)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@arraxu.local>
;; Keywords: text, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; PlantUML offers two flavors of server-based rendering, here we test against
;; both of them

;;; Code:

(ert-deftest plantuml-server-test/server-available ()
  "Just to see if tagging works."
  :tags '(server)

  (plantuml-test-server
   (lambda ()
     (plantuml-server-get-language (current-buffer)))))

(ert-deftest plantuml-server-test/hex-encoding ()
  "Test HEX encoding."
  :tags '(server)

  (plantuml-test-server
   (lambda ()
     (setq-local plantuml-server-encode-mode 'hex)
     (plantuml-server-preview-string 0 "@startuml\nA -> B\n@enduml" (current-buffer))

     (should (equal (concat "     ┌─┐          ┌─┐\n"
                            "     │A│          │B│\n"
                            "     └┬┘          └┬┘\n"
                            "      │            │ \n"
                            "      │───────────>│ \n"
                            "     ┌┴┐          ┌┴┐\n"
                            "     │A│          │B│\n"
                            "     └─┘          └─┘\n")
                    (buffer-string))))))

(ert-deftest plantuml-server-test/deflate-encoding ()
  "Test DEFLATE encoding."
  :tags '(server)

  (plantuml-test-server
   (lambda ()
     (setq-local plantuml-server-encode-mode 'deflate)
     (plantuml-server-preview-string 0 "@startuml\nA -> B\n@enduml" (current-buffer))

     (should (equal (concat "     ┌─┐          ┌─┐\n"
                            "     │A│          │B│\n"
                            "     └┬┘          └┬┘\n"
                            "      │            │ \n"
                            "      │───────────>│ \n"
                            "     ┌┴┐          ┌┴┐\n"
                            "     │A│          │B│\n"
                            "     └─┘          └─┘\n")
                    (buffer-string))))))



(provide 'plantuml-server-test)
;;; plantuml-server-test.el ends here

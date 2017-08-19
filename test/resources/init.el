;; Used to test `plantuml-mode' installations

(setq user-emacs-directory "./.emacs.d")

(require 'subr-x)
(setq local-repository
      (concat
       (string-trim (shell-command-to-string "cask package-directory"))
       "/../testing"))
(custom-set-variables `(package-archive-upload-base ,local-repository))

(require 'package-x)
(defun -package-upload (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((pkg-desc (package-buffer-info)))
      (package-upload-buffer-internal pkg-desc "el"))))
(-package-upload "plantuml-mode.el")

(setq package-archives `(("local" . ,local-repository)))
(package-initialize)

(package-install 'plantuml-mode)
(require 'plantuml-mode)

(message
 (concat "Successfully installed plantuml-mode v" plantuml-mode-version))

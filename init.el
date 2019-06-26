;; -*- eval: (git-auto-commit-mode 1) -*-
;; This configuration is inspired heavily from Mike Zamansky's blog
;; http://cestlaz.github.io/stories/emacs/

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 's)
(require 'f)
;; (require 'ht)
(require 'git)
(require 'ert)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file user-emacs-directory)))

(dolist module (f-files "modules" ))






;; (load-local "mods")

;; (dolist elisp-mod reve:mods
;;         (load-local elisp-mod))

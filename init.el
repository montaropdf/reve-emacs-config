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

(setq config-unit-location (f-expand "codes/reve-elisp/reve-econfig/modules" default-directory))
(setq config-file-name "local.config.el")
(setq config-file-location (f-expand config-file-name user-emacs-directory))

(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file user-emacs-directory)))

(unless (f-exists? config-file-location)
  (dolist (unit (f-files config-unit-location))
    (org-babel-tangle-file unit config-file-location)))

(load-local config-file-name)

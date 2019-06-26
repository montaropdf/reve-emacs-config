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

(defvar config-unit-location (f-expand "codes/reve-elisp/reve-econfig/modules" default-directory)
  "Location of the configuration units to be processed to create the final configuration file.")
(defvar config-file-name "local.config.el"
  "Name of the final configuration file to load.")
(defvar config-file-location (f-expand config-file-name user-emacs-directory)
  "Location of the final configuration file to load.")

(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file user-emacs-directory)))

(unless (f-exists? config-file-location)
  (progn
    (org-babel-tangle-file (f-join config-unit-location "pre-config.org") config-file-location)
    (dolist (unit (f-files config-unit-location))
      (unless (or (eq unit "pre-config.org") (eq unit "post-config.org"))
        (org-babel-tangle-file unit config-file-location)))
    (org-babel-tangle-file (f-join config-unit-location "post-config.org") config-file-location)))

(load-local config-file-name)

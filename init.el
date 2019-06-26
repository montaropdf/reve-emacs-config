;; -*- eval: (git-auto-commit-mode 1) -*-
;; This configuration is inspired heavily from Mike Zamansky's blog
;; http://cestlaz.github.io/stories/emacs/

;; * Cask initialisation and require calls
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 's)
(require 'f)
;; (require 'ht)
;; (require 'git)
;; (require 'ert)
(require 'use-package)

;; * Variables
(setq default-directory (f-full (getenv "HOME")))

(defvar reve:target-directory user-emacs-directory
  "The directory that will contain the final configuration file.")
(defvar reve:config-unit-location (f-expand "modules" default-directory)
  "Location of the configuration units to be processed to create the final configuration file.")
(defvar reve:config-file-name "local.config.el"
  "Name of the final configuration file to load.")
(defvar reve:config-file-location (f-expand config-file-name user-emacs-directory)
  "Location of the final configuration file to load.")
(defvar reve:pre-config-unit (f-join config-unit-location "pre-config.org")
  "Full path of the pre-config configuration unit.")
(defvar reve:post-config-unit (f-join config-unit-location "post-config.org")
  "Full path of the post-config configuration unit.")

;; * Functions
(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file user-emacs-directory)))

(defun tangle-or-insert (config-unit)
  "Check the extension of CONFIG-UNIT and decide if it must be tangled or inserted in the final configuration file."
  (cond ((f-ext? config-unit "org")
         (org-babel-tangle-file config-unit config-file-location))
        ((f-ext? config-unit "el")
         (let ((config-unit-buffer (find-file-noselect config-unit)))
           (progn
             (with-current-buffer config-unit-buffer
               (append-to-file (point-min) (point-max) config-file-location))
             (kill-buffer config-unit-buffer))))
        (t
         (message "Unknown extension types."))))

;; * Main Process
(unless (f-exists? config-file-location)
  (progn
    (when (f-exists? pre-config-unit)
      (tangle-or-insert pre-config-unit))
    (dolist (unit (f-files config-unit-location))
      (unless (or (eq unit "pre-config.org") (eq unit "post-config.org"))
        (tangle-or-insert unit)))
    (when (f-exists? post-config-unit)
      (tangle-or-insert post-config-unit))))

(load-local config-file-name)

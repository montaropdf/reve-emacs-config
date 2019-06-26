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
(defvar pre-config-unit (f-join config-unit-location "pre-config.org")
  "Full path of the pre-config configuration unit.")
(defvar post-config-unit (f-join config-unit-location "post-config.org")
  "Full path of the post-config configuration unit.")

(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file user-emacs-directory)))

(defun tangle-or-insert (config-unit)
  "Check the extension of CONFIG-UNIT and decide if it must be tangled or inserted in the final configuration file."
  (cond ((f-ext? config-unit "org")
         (org-babel-tangle-file pre-config-unit config-file-location))
        ((f-ext? config-unit "el")
         (let ((config-unit-buffer (find-file-noselect config-unit)))
           (progn
             (with-current-buffer config-unit-buffer
               (append-to-file (point-min) (point-max) config-file-location))
             (kill-buffer config-unit-buffer))))
        (t
         (message "Unknown extension types."))))


(unless (f-exists? config-file-location)
  (progn
    (when (f-exists? pre-config-unit)
      (org-babel-tangle-file pre-config-unit config-file-location))
    (dolist (unit (f-files config-unit-location))
      (unless (or (eq unit "pre-config.org") (eq unit "post-config.org"))
        (org-babel-tangle-file unit config-file-location)))
    (when (f-exists? post-config-unit)
      (org-babel-tangle-file post-config-unit config-file-location))))

(load-local config-file-name)

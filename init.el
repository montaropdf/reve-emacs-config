;; -*- eval: (git-auto-commit-mode 1) -*-
;; This configuration is inspired heavily from Mike Zamansky's blog
;; http://cestlaz.github.io/stories/emacs/

<<<<<<< HEAD
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
(defvar reve:config-file-location (f-expand reve:config-file-name reve:target-directory)
  "Location of the final configuration file to load.")
(defvar reve:pre-config-unit (f-join reve:config-unit-location "pre-config.org")
  "Full path of the pre-config configuration unit.")
(defvar reve:post-config-unit (f-join reve:config-unit-location "post-config.org")
  "Full path of the post-config configuration unit.")

;; * Functions
(defun load-local (file)
  "Load FILE assuming it is located in the path stored in USER-EMACS-DIRECTORY."
  (load (f-expand file reve:target-directory)))

(defun tangle-or-insert (config-unit)
  "Check the extension of CONFIG-UNIT and decide if it must be tangled or inserted in the final configuration file."
  (cond ((f-ext? config-unit "org")
         (org-babel-tangle-file config-unit reve:config-file-location))
        ((f-ext? config-unit "el")
         (let ((config-unit-buffer (find-file-noselect config-unit)))
           (progn
             (with-current-buffer config-unit-buffer
               (append-to-file (point-min) (point-max) reve:config-file-location))
             (kill-buffer config-unit-buffer))))
        (t
         (message "Unknown extension types."))))

;; * Main Process
(unless (f-exists? reve:config-file-location)
  (progn
    (when (f-exists? reve:pre-config-unit)
      (tangle-or-insert reve:pre-config-unit))
    (dolist (unit (f-files reve:config-unit-location))
      (unless (or (eq unit "pre-config.org") (eq unit "post-config.org"))
        (tangle-or-insert unit)))
    (when (f-exists? reve:post-config-unit)
      (tangle-or-insert reve:post-config-unit))))

(load-local config-file-name)
=======
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))

(package-initialize)                ;; Initialize & Install Package

; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.config/emacs/init.emacs.org"))
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets x509-mode which-key weechat virtualenvwrapper use-package unidecode unicode-whitespace unicode-progress-reporter unicode-input unicode-fonts unicode-escape unicode-enbox unicode-emoticons undo-tree try treemacs-projectile spaceline-all-the-icons slime-company simple-mpc shx sauron ranger plantuml-mode persp-mode pcre2el pass parsec ox-reveal ox-mediawiki org-transform-tree-table org-timeline org-super-agenda org-plus-contrib org-pdfview org-mind-map org-journal org-clock-today org-bullets org-brain org-board org-alert org-ac nov mu4e-maildirs-extension mu4e-alert mingus magit lxc-tramp lxc load-theme-buffer-local load-bash-alias ivy-pass ivy-gitlab iedit hungry-delete htmlize gitlab-ci-mode-flycheck gited gitconfig-mode git-timemachine git-gutter git-blamed git-auto-commit-mode expand-region eshell-prompt-extras eshell-git-prompt eshell-did-you-mean eshell-bookmark eshell-autojump esh-help esh-buf-stack esh-autosuggest elpy elfeed-org elfeed-goodies eldoc-eval doom-themes doom-modeline darktooth-theme darkokai-theme darkmine-theme darkburn-theme cyberpunk-theme csv-mode crux counsel cask-package-toolset cask-mode cask calfw-org calfw-cal calfw beacon badger-theme atom-one-dark-theme atom-dark-theme arjen-grey-theme apropospriate-theme ample-zen-theme ample-theme all-the-icons-ivy all-the-icons-dired alect-themes aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
>>>>>>> e3e60cf... Current configuration

;; -*- eval: (git-auto-commit-mode 1) -*-
;; This configuration is inspired heavily from Mike Zamansky's blog
;; http://cestlaz.github.io/stories/emacs/

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
<<<<<<< HEAD
>>>>>>> e3e60cf... Current configuration
=======
>>>>>>> 630c3375bf68b41b1cbed86efd0f699f3ab95c13

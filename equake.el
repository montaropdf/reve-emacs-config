(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))

(package-initialize)                ;; Initialize & Install Package

                                        ; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package equake
  :ensure t
  :config  ; some examples of optional settings follow:
  (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing) ; prevent accidental frame-closure
  (setq equake-size-width 0.99) ; set width a bit less than full-screen (prevent 'overflow' on multi-monitor)
  ;; set distinct face for Equake: white foreground with dark blue background, and different font
  (set-face-attribute 'equake-buffer-face 'nil :inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white"))

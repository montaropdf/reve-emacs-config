(setenv "XDG_CONFIG_HOME" (concat (getenv "HOME") "/.config"))

(defvar emacs-root-cfg-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs"))
(defvar emacs-base-cfg-dir (concat emacs-root-cfg-dir "/base"))
(defvar emacs-org-cfg-dir (concat emacs-root-cfg-dir "/organization"))
(defvar emacs-user-cfg-dir (concat emacs-root-cfg-dir "/user"))
(defvar emacs-machine-cfg-dir (concat emacs-root-cfg-dir "/machine"))
(defvar emacs-configuration-tags-list '())
(defvar emacs-configuration-categories-list '())

(defun load-if-exists (f)
  "Load the file, if it exist and is readable."
  (when (file-readable-p f) (load-file (expand-file-name f))))

(defun org-babel-load-if-exists (f)
  "Load the org file, if it exist and is readable, using org babel."
  (when (file-readable-p f) (org-babel-load-file (expand-file-name f))))

(load-if-exists (concat emacs-org-cfg-dir "/proxy.el"))
(setq custom-file (concat emacs-user-cfg-dir "/custom.el"))
(load-if-exists custom-file)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)                ;; Initialize & Install Package

; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(require 'f)

(defun reve:get-lisp-forms (filename)
  "Read FILENAME and return a list of its Lisp forms."
  (with-temp-buffer
    (let ((buf (current-buffer))
          forms)
      (insert-file-contents filename)
      (while (ignore-errors (push (read buf) forms)))
      (nreverse forms)
      forms)))

(defun reve:get-config-element (element-a element-b)
  "Return one of the element."
  (cond ((= (elt element-a 0) ?+)
	 element-a)
	((= (elt element-a 0) ?-)
	 (if (= (elt element-b 0) ?+)
	     element-b)
	   element-a)
	(t
	 (cond ((= (elt element-b 0) ?+)
		element-b)
	       ((= (elt element-b 0) ?-)
		element-b)
	       (t
		element-a)))))


(defun reve:config-list-merge (list1 list2)
  "Return the intersection of the 2 configuration list."
  (let ((config-element-merge-list '())
	(list-a (cdr list1))
	(list-b (cdr list2))
	(element-a)
	(element-b))
    (dotimes (index (length list-a))
      (setq element-a (nth index list-a))
      (setq element-b (nth index list-b))
      (setq config-element-merge-list
	    (append config-element-merge-list
		    (list (reve:get-config-element element-a
						   element-b)))))
    config-element-merge-list))

(defun reve:get-final-config-list (config-list)
  "Return the list of configuration elements to be processed."
  (let ((final-list '()))
    (dolist (element config-list final-list)
      (cond ((= (elt element 0) ?+)
	     (setq final-list
		   (append final-list
			   (list (seq-drop element 1)))))
	    ((not (= (elt element 0) ?-))
	     (setq final-list
		   (append final-list
			   (list element))))))))

(defun reve:sections-config-list-merge (config-list-name)
  "Gather the configuration list for all sections for list CONFIG-LIST-NAME."
  (let ((cfg-list '())
        (compiled-list '())
	(revecloud-env-filename))
    (dolist (section '("base" "organization" "user" "machine"))
      (setq revecloud-env-filename (f-expand (f-join section
						     ".revecloud-env.el")
					     emacs-root-cfg-dir))
      (when (file-readable-p revecloud-env-filename)
	(setq cfg-list
	      (cdr (assoc config-list-name
			  (car (reve:get-lisp-forms
				revecloud-env-filename)))))
	(if compiled-list
	    (setq compiled-list
		  (reve:config-list-merge compiled-list
					  cfg-list))
	  (setq compiled-list cfg-list))))
    (reve:get-final-config-list compiled-list)))

(setq emacs-configuration-categories-list (reve:sections-config-list-merge 'revecloud-category))
(setq emacs-configuration-tags-list (reve:sections-config-list-merge 'revecloud-tags))

(dolist (elt emacs-configuration-categories-list)
  ;; Load configuration of the module from the [base] configuration
  (unless (org-babel-load-if-exists (concat emacs-base-cfg-dir "/" elt ".org"))
    (load-if-exists (concat emacs-base-cfg-dir "/" elt ".el")))

  ;; Load configuration of the module from the [organization] configuration
  (unless (org-babel-load-if-exists (concat emacs-org-cfg-dir "/" elt ".org"))
    (load-if-exists (concat emacs-org-cfg-dir "/" elt ".el")))

  ;; Load configuration of the module from the [user] configuration
  (unless (org-babel-load-if-exists (concat emacs-user-cfg-dir "/" elt ".org"))
    (load-if-exists (concat emacs-user-cfg-dir "/" elt ".el")))

  ;; Load configuration of the module from the [machine] configuration
  (unless (org-babel-load-if-exists (concat emacs-machine-cfg-dir "/" elt ".org"))
    (load-if-exists (concat emacs-machine-cfg-dir "/" elt ".el"))))

(put 'narrow-to-region 'disabled nil)

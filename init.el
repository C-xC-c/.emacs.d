;; required package fluff
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(defun in-emacs-home (file)
  (when (stringp file)
    (let ((file (concat user-emacs-directory file)))
      (when (file-exists-p file)
	file))))

(setq custom-file (in-emacs-home "custom.el"))
(defconst emacs-config (in-emacs-home "config.el"))
(defconst emacs-org (in-emacs-home "config.org"))

;; org-babel won't compile these sometimes?
(load emacs-config)
(load custom-file 'noerrror)

(when (file-newer-than-file-p emacs-org emacs-config)
  (org-babel-load-file emacs-org))


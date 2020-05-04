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
	"Checks if a file exists in your emacs home"
	(when (stringp file)
		(let ((file (concat user-emacs-directory file)))
			(when (file-exists-p file)
				file))))

;; Files we care about loading
(setq custom-file (in-emacs-home "custom.el"))
(defconst emacs-config (in-emacs-home "config.el"))
(defconst emacs-org (in-emacs-home "config.org"))
(defconst emacs-email (in-emacs-home "email.el"))

;; If everything exists then execute files
(unless (member nil '(custom-file emacs-config emacs-org emacs-email))
	(if (file-newer-than-file-p emacs-org emacs-config)
			(org-babel-load-file emacs-org)
		(load emacs-config))
	(load custom-file 'noerrror)
	(load emacs-email 'noerror))

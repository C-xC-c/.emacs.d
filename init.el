(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun check-installed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(mapc 'check-installed '(use-package spacemacs-theme))

(defun in-emacs-home (file)
  "Checks if a file exists in your emacs home"
  (when (stringp file)
    (let ((file (concat user-emacs-directory file)))
      (when (file-exists-p file)
        file))))

;; Files we care about loading
(setq custom-file (in-emacs-home "custom.el"))
(defconst manx/emacs-el (in-emacs-home "config.el"))
(defconst manx/emacs-org (in-emacs-home "config.org"))
(defconst manx/emacs-email (in-emacs-home "email.el"))
(defconst manx/org-export (in-emacs-home "export.el"))

(when (and manx/emacs-el manx/emacs-org)
  (if (file-newer-than-file-p manx/emacs-org manx/emacs-el)
      (org-babel-load-file manx/emacs-org)
    (load manx/emacs-el 'noerror)))

(mapc (lambda (file)
        (when file
          (load file 'noerror)))
      '(manx/emacs-email manx/org-export custom-file))

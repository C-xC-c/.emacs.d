(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun check-installed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(mapc 'check-installed '(use-package spacemacs-theme))

(defun in-emacs-home (file)
  "Returns `file' if it exists in `user-emacs-directory'"
  (declare (type string file))
  (let ((home-file (concat user-emacs-directory file)))
    (when (file-exists-p home-file)
      home-file)))

;; Files we care about loading
(setq custom-file (in-emacs-home "custom.el"))
(defconst manx/emacs-el (in-emacs-home "config.el"))
(defconst manx/emacs-org (in-emacs-home "config.org"))
(defconst manx/emacs-email (in-emacs-home "email.el"))
(defconst manx/org-export (in-emacs-home "org-export.el"))

(cond
 ((and manx/emacs-el manx/emacs-org)
  (if (file-newer-than-file-p manx/emacs-org manx/emacs-el)
      (org-babel-load-file manx/emacs-org)
    (load manx/emacs-el 'noerror)))
 ((manx/emacs-org)
  (org-babel-load-file manx/emacs-org)))

(mapc (lambda (file)
        (when file
          (load file 'noerror)))
      (list manx/emacs-email manx/org-export custom-file))

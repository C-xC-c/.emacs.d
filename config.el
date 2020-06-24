(defmacro definteractive (name &rest body)
  `(defun ,name ,(car body)
     (interactive)
     ,@(cdr body)))

(defmacro lambdainteractive (&rest body)
  `(lambda ,(car body) (interactive) ,@(cdr body)))

(defmacro local-keybind (key value)
  `(lambda () (local-set-key (kbd ,key) (quote ,value))))

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package keychain-environment
  :bind ("C-c r e" . 'keychain-refresh-environment)
  :init (keychain-refresh-environment))

(use-package ox-slimhtml)
(org-export-define-derived-backend 'custom-html-exporter
    'slimhtml
  :translate-alist
  '((code . org-html-code)
    (timestamp . org-html-timestamp)))

(use-package projectile
  :config (projectile-mode 1))

(use-package diminish
  :init
  (diminish 'eldoc-mode)
  (diminish 'org-src-mode))

(use-package auto-minor-mode)

(use-package company
  :diminish 'company-mode
  :bind (:map company-mode-map
              ("C-c /" . 'yas-expand))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package csharp-mode
  :defer t
  :config
  (use-package omnisharp
    :defer t
    :config
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)))

(use-package elixir-mode
  :defer t
  :config (use-package alchemist))

(use-package nginx-mode
  :custom
  (nginx-indent-tabs-mode t)
  (nginx-indent-level 2)
  :config (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package slime
  :defer t
  :custom
  (slime-lisp-implementations
   '((sbcl ("sbcl" "--core" "/usr/lib64/sbcl/sbcl.core")
           :env ("SBCL_HOME=/usr/lib64/sbcl/"))))
  :diminish (slime-mode slime-autodoc-mode)
  :init
  (use-package slime-company)
  (add-hook 'lisp-mode-hook 'slime-mode)
  (slime-setup '(slime-fancy slime-company)))

(use-package htmlize)

(use-package yasnippet
  :diminish 'yas-minor-mode
  :hook ((html-mode
          LaTeX-mode
          emacs-lisp-mode
          lisp-mode)
         . yas-minor-mode)
  :init ;; These are computationally expensive, so init.
  (use-package yasnippet-snippets)
  (yas-reload-all))

(use-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-banner-logo-title "Komacs")
  (dashboard-set-init-info t)
  (dashboard-startup-banner "~/.emacs.d/Komacs.png")
  (dashboard-show-shortcuts nil)
  (dashboard-items '((recents . 5)))
  :config (dashboard-setup-startup-hook))

(use-package transpose-frame
  :ensure t
  :bind (("C-c f t" . transpose-frame)
         ("C-c f i" . flip-frame)
         ("C-c f o" . flop-frame)))

(use-package switch-window
  :custom
  (switch-window-input-style 'minibuffer)
  (switch-window-increase 4)
  (switch-window-threshold 2)
  :bind ([remap other-window] . switch-window))

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-everywhere 1)

(use-package ido-vertical-mode
  :bind ("C-l" . 'ido-reread-directory)
  :custom (ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 1)
  (ido-mode 1))

(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  :config
  (spaceline-toggle-buffer-size-off)
  ;;This isn't set in :custom because it breaks the arrow.
  (setq powerline-default-seperator 'arrow)
  (add-hook 'after-init-hook 'spaceline-compile))

(use-package hungry-delete
  :diminish 'hungry-delete-mode
  :config (global-hungry-delete-mode 1))

(use-package which-key
  :diminish 'which-key-mode
  :config (which-key-mode))

(use-package avy
  :bind ("M-s" . avy-goto-char))

(use-package beacon
  :diminish 'beacon-mode
  :config (beacon-mode 1))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

(definteractive manx/config-reload ()
  (when (get-buffer "config.org")
    (with-current-buffer "config.org" (save-buffer)))
  (org-babel-load-file manx/emacs-org))

(global-set-key (kbd "C-c x r") 'manx/config-reload)
(global-set-key (kbd "C-c x e") (lambdainteractive () (find-file manx/emacs-org)))

(setq org-src-window-setup 'current-window)

;; I read somewhere that Company breaks things?
(add-hook 'org-mode-hook 'company-mode)

;; Don't indent whole file with org-mode
(eval-after-load "org-mode" (local-set-key (kbd "s-i") nil))

(setq org-structure-template-alist
      (append
       '(("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
				 ("lisp" "#+BEGIN_SRC lisp\n?\n#+END_SRC")
				 ("sh" "#+BEGIN_SRC shell\n?\n#+END_SRC"))
       org-structure-template-alist))

(setq org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-src-preserve-indentation nil
      org-agenda-files '("~/todo.org"))

(setq org-html-doctype "html5")

(defun manx/eval-these (lang body)
  (not (string-equal "\"\\n\"" body)))

(setq org-confirm-babel-evaluate 'manx/eval-these)

(defun manx-publish/local-dir (dir)
  (concat "~/Documents/org/" dir))

(defun manx-publish/remote-dir (dir)
  (concat "/ssh:plum@plum.moe|sudo:78:" dir))

(defvar manx-publish/html-head "<link rel=\"stylesheet\" href=\"/static/css/style.css\" />")

(defun sitemap (title list)
  (concat "#+title:" title "\n"
          "#+setupfile:~/Documents/org/includes/setup.org\n\n"
          (format "@@html:<h1>%s</h1>@@" title)
          "@@html:<archive>@@"
          (string-join (mapcar #'car (cdr list)))
          "@@html:</archive>@@"))

(setq org-publish-project-alist
      `(("plum"
         :base-directory ,(manx-publish/local-dir "plum")
         :publishing-directory ,(manx-publish/remote-dir "/var/www/plum.moe")
         :publishing-function ox-slimhtml-publish-to-html
         :html-head ,manx-publish/html-head
         :recursive t)
        ("words"
         :base-directory ,(manx-publish/local-dir "words")
         :publishing-directory ,(manx-publish/remote-dir "/var/www/words.plum.moe")
         :publishing-function ox-slimhtml-publish-to-html
         :auto-sitemap t
         :sitemap-filename "index.html"
         :sitemap-title "Words by Manx"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d - %t"
         :sitemap-function sitemap
         :author-info t
         :creator-info t
         :html-head ,manx-publish/html-head)
        ("flags"
         :base-directory ,(manx-publish/local-dir "flags")
         :publishing-directory ,(manx-publish/remote-dir "/var/www/flags")
         :publishing-function ox-slimhtml-publish-to-html
         :html-head ,manx-publish/html-head)
        ("static"
         :base-directory ,(manx-publish/local-dir "static")
         :base-extension "css\\|js\\|svg"
         :publishing-function org-publish-attachment
         :recursive t
         :publishing-directory ,(manx-publish/remote-dir "/var/www/plum.moe/static"))))

(definteractive manx/blog ()
  (load-theme 'spacemacs-light)
  (org-publish-project "words")
  (load-theme 'spacemacs-dark))

(definteractive manx/delete-org-link ()
  (when (org-in-regexp org-bracket-link-regexp 1)
    (apply 'delete-region (list (match-beginning 0) (match-end 0)))))

(add-hook 'org-mode-hook (local-keybind "C-c o l" manx/delete-org-link))

(setq org-image-actual-width 150)

(definteractive manx/org-insert-link ()
  (org-insert-link)
  (org-redisplay-inline-images))

(add-hook 'org-mode-hook (local-keybind "C-c C-l" manx/org-insert-link))

(definteractive manx/kill-line()
  (move-beginning-of-line nil)
  (kill-whole-line))

(definteractive manx/format-whole-buffer()
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c M-w") (lambdainteractive () (kill-ring-save (point-min) (point-max))))
(global-set-key (kbd "C-c k l") 'manx/kill-line)
(global-set-key (kbd "s-i") 'manx/format-whole-buffer)
(global-set-key (kbd "C-c r b") 'revert-buffer)
(global-set-key (kbd "<M-right>") 'forward-whitespace)

(definteractive manx/scratch-buffer ()
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(definteractive manx/lisp-buffer ()
  (switch-to-buffer (get-buffer-create "*lisp playground*"))
  (lisp-mode))

(definteractive manx/kill-all ()
  (mapc 'kill-buffer (buffer-list))
  (manx/scratch-buffer))

(global-set-key (kbd "C-c s b") 'manx/scratch-buffer)
(global-set-key (kbd "C-x k") (lambdainteractive () (kill-buffer (current-buffer))))
(global-set-key (kbd "C-M-s-k") 'manx/kill-all)

(defmacro manx/split-and-follow (direction)
	`(progn
		       ,direction
		       (balance-windows)
		       (other-window 1)))

(global-set-key (kbd "C-x 3")
								(lambdainteractive () (manx/split-and-follow (split-window-below))))
(global-set-key (kbd "C-x 2")
								(lambdainteractive () (manx/split-and-follow (split-window-horizontally))))

(definteractive manx/theme ()
  "Themes don't load properly when using emacsclient"
  (load-theme 'spacemacs-dark)
  (spaceline-compile))

(defun unix-line-ends ()
  (when (string-match
         "-\\(?:dos\\|mac\\)$"
         (symbol-name buffer-file-coding-system))
    (set-buffer-file-coding-system 'unix)))

(add-hook 'find-file-hooks 'unix-line-ends)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(line-number-mode 1)
(column-number-mode 1)
(display-battery-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100
      select-enable-clipboard t
      vc-follow-symlinks t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "basilisk")

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "autosaves"))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell nil
      ring-bell-function 'ignore)

(global-unset-key (kbd "C-z")) ;; Fuck unix

(setq-default tab-width 2
              indent-tabs-mode t)

(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(defvaralias 'css-indent-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

(global-prettify-symbols-mode t)

(defmacro manx/prettify (lst)
  `(add-hook
    (quote ,(car lst))
    (lambda ()
      (dolist (pair (quote ,(cdr lst)))
        (push pair prettify-symbols-alist)))))

(manx/prettify
 (emacs-lisp-mode-hook
  ("lambdainteractive" . ?Λ)))

(manx/prettify
 (prog-mode-hook
  ("||" . ?∨)
  ("&&" . ?∧)
  ("!=" . ?≠)))

(manx/prettify
 (js-mode-hook
  ("=>" . ?⇒)))

(define-minor-mode sensitive-minor-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :init-value nil
  :lighter " Sensitive"
  :keymap nil 
  (if (symbol-value sensitive-minor-mode)
      (progn
        (setq make-backup-files nil)
        (auto-save-mode -1))
    (setq-local make-backup-files t)
    (auto-save-mode 1)))


;; Regexps of sensitive files.
(setq auto-minor-mode-alist
      (append
       '(("stream/manifest/.*\\.json$" . sensitive-minor-mode)
         (".emacs.d/snippets/\\*$" . sensitive-minor-mode)
         ("/etc/nginx/*" . sensitive-minor-mode))
       auto-minor-mode-alist))

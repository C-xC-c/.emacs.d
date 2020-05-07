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

(use-package diminish)

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
  :diminish 'slime-mode
  :init
  (use-package slime-company)
  (add-hook 'lisp-mode-hook 'slime-mode)
  (slime-setup '(slime-fancy slime-company)))

(use-package keychain-environment
  :bind ("C-c r e" . 'keychain-refresh-environment)
  :init (keychain-refresh-environment))

(use-package htmlize)

(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  :config
  (spaceline-toggle-buffer-size-off)
  (setq powerline-default-seperator (quote arrow)))

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

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package switch-window
  :custom
  (switch-window-input-style 'minibuffer)
  (switch-window-increase 4)
  (switch-window-threshold 2)
  :bind ([remap other-window] . switch-window))

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

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-everywhere 1)

(use-package ido-vertical-mode
  :bind ("C-l" . 'ido-reread-directory)
  :custom (ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 1)
  (ido-mode 1))

(definteractive manx/config-reload ()
  (when (get-buffer "config.org")
    (with-current-buffer "config.org" (save-buffer)))
  (org-babel-load-file (concat user-emacs-directory "config.org")))

(global-set-key (kbd "C-c x r") 'manx/config-reload)
(global-set-key (kbd "C-c x e") (lambdainteractive () (find-file (concat user-emacs-directory "config.org"))))

(setq org-src-window-setup 'current-window)

;; I read somewhere that Company breaks things?
(add-hook 'org-mode-hook 'company-mode)

;; Don't indent whole file with org-mode
(eval-after-load "org-mode" (local-set-key (kbd "s-i") nil))

(add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(setq org-html-doctype "html5")

(definteractive manx/save-org-to-html()
  (when (equal major-mode 'org-mode)
    (save-buffer)
    (org-html-export-to-html)))

(add-hook 'org-mode-hook
          (local-keybind "C-c s h" manx/save-org-to-html))

(definteractive manx/delete-org-link ()
  (when (org-in-regexp org-bracket-link-regexp 1)
    (apply 'delete-region (list (match-beginning 0) (match-end 0)))))

(add-hook 'org-mode-hook (local-keybind "C-c o l" manx/delete-org-link))

(setq org-image-actual-width 150)

(definteractive manx/org-insert-link ()
  (org-insert-link)
  (org-redisplay-inline-images))

(add-hook 'org-mode-hook (local-keybind "C-c C-l" manx/org-insert-link))

(definteractive manx/kill-word ()
  (backward-word)
  (kill-word 1))

(definteractive manx/kill-line()
  (move-beginning-of-line nil)
  (kill-whole-line))

(definteractive manx/format-whole-buffer()
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "<M-right>") 'forward-whitespace)
(global-set-key (kbd "C-c r b") 'revert-buffer)
(global-set-key (kbd "C-c k w") 'manx/kill-word)
(global-set-key (kbd "C-c k l") 'manx/kill-line)
(global-set-key (kbd "s-i") 'manx/format-whole-buffer)

(global-set-key (kbd "C-x k") (lambdainteractive () (kill-buffer (current-buffer))))
(global-set-key (kbd "C-M-s-k") (lambdainteractive () (mapc 'kill-buffer (buffer-list))))

;; This is only used here for now but we should still more it some
;; time
(add-to-list 'load-path "~/.emacs.d/scripts/")

(require 'transpose-frame)
(global-set-key (kbd "C-c f t") 'transpose-frame)
(global-set-key (kbd "C-c f i") 'flip-frame)
(global-set-key (kbd "C-c f o") 'flop-frame)

(defmacro manx/split-and-follow (direction)
  `(progn
     ,direction
    (balance-windows)
    (other-window 1)))

(global-set-key (kbd "C-x 3") (lambdainteractive () (manx/split-and-follow (split-window-below))))
(global-set-key (kbd "C-x 2") (lambdainteractive () (manx/split-and-follow (split-window-horizontally))))

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
(electric-pair-mode t)
(global-hl-line-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100
      select-enable-clipboard t
      vc-follow-symlinks t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "autosaves"))))

(global-unset-key (kbd "C-z")) ;; Fuck unix

(setq-default tab-width 2
              indent-tabs-mode t)

(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(defvaralias 'css-indent-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

(global-prettify-symbols-mode t)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push
             '("lambdainteractive" . ?Λ)
             prettify-symbols-alist)))

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "Check file name against `auto-minor-mode-alist' to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name (file-name-sans-versions buffer-file-name))
          (remote-id (file-remote-p buffer-file-name))
          (case-fold-search auto-mode-case-fold)
          (alist auto-minor-mode-alist))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook #'enable-minor-mode-based-on-extension)

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
         ("nginx/sites-(enabled|available)/*" . sensitive-minor-mode))
       auto-minor-mode-alist))

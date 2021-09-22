								; Emacs Core
(message "---------- NEW init.el LOAD ----------")

;; Local plugins
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq machine-specific-file
	 (concat
	  (expand-file-name "~/.emacs.d/machines/")
	  (concat (system-name) ".el")))
(if (file-exists-p machine-specific-file) 
    (load machine-specific-file))

;; Built in package manager
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Use Package
(eval-when-compile
  (require 'use-package))

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Font
;; (setq default-frame-alist '((font . "Hack-12")))
(if (boundp 'my-font-size) (set-face-attribute 'default nil :height my-font-size))

;; Tab width
(setq-default tab-width 5)

								; Colors
;;;(load-theme 'zenburn t)
;;;(load-theme 'tangotango t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
;(set-face-foreground 'font-lock-comment-face "green")

								; Behavior
;; Auto Save
;;; Save all backup files in a dedicated directory
;;; https://stackoverflow.com/a/2680682
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
)

;;; Place autosave files in dedicated directory
;;; https://www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup"))))

;; C-<return> = Exit search at beginning of match
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

;; Editor config
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; Line numbers
(global-linum-mode)

;; Max line length
;;; (use-package column-marker)
;;; (add-hook 'text-mode-hook (lambda () (interactive) (column-marker-3 80)))
;;; (add-hook 'go-mode-hook (lambda () (interactive) (column-marker-3 80)))

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)

;; Highlight hex colors
(use-package rainbow-mode
  :ensure t)
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
 (lambda () (rainbow-mode t)))

(my-global-rainbow-mode t)

;; New line at end of file
(setq require-final-newline 'visit-save)

;; Disable top menu bar in tty mode
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Window movements
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Transpose split windows
(use-package transpose-frame
  :ensure t
  :bind ("C-x /" . transpose-frame))

;; Auto indent
;; (setq auto-indent-on-visit-file t)
;; (require 'auto-indent-mode)

;; Rename file
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; C-x x Save and kill buffer
(defun save-and-kill-buffer ()
  "Save and kill a buffer, similar to save-buffers-kill-terminal but only kills the
current buffer."
  (interactive)
  (progn (save-buffer)
	    (kill-buffer)))
(global-set-key (kbd "C-c x") 'save-and-kill-buffer)

;; Auto complete
;; (require 'company)
;; (require 'company-lsp)
;; (require 'lsp-mode)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (push 'company-lsp company-backends)

;; (add-hook 'go-mode-hook 'lsp-mode)

;; Show function arguments on hover
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
;; (add-hook 'ielm-mode-hook 'eldoc-mode)

;; Org mode
(if (boundp 'my-org-agenda-files)
    (setq org-agenda-files (mapcar 'expand-file-name my-org-agenda-files)))
(setq org-todo-keywords
	 '((sequence "TODO" "DOING" "PAUSED" "|" "DONE" "ABANDONED")))

(setq org-tempo-keywords-alist
	 '(("L" . "latex")
	  ("H" . "html")
	  ("A" . "ascii")
	  ("i" . "index")
	  ("s" . "src")))

(use-package org-bullets
  :ensure t
  :custom
  (inhibit-compacting-font-caches t) ; https://github.com/integral-dw/org-bullets#this-mode-causes-significant-slowdown
  :hook (org-mode . org-bullets-mode))

;; Git
;;; Magit extension
(use-package magit
  :ensure t
  :bind (:map magit-file-section-map
              ("RET" . magit-diff-visit-file-other-window)
              :map magit-hunk-section-map
              ("RET" . magit-diff-visit-file-other-window))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; Make ediff navigation window not open in new window
  )

;; Organize buffers by project
(use-package projectile
  :ensure t
  :config (projectile-mode +1))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Ensure not garbage collecting too quickly
;;; I found in large org mode files the input lag was very high. After googling I found out that this is bc Emacs was garbage collecting after every keystroke. This Reddit post suggested setting a minimum time of 5 seconds between gc rounds and setting a higher memory cap.
;;; Original googled Reddit post: https://www.reddit.com/r/emacs/comments/6uhzc9/very_slow_org_mode/
;;; Linked to this comment: https://www.reddit.com/r/emacs/comments/6uhzc9/very_slow_org_mode/dlwyzmg/
;;; Links to this og Reddit post: https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/d8cmm7v/
(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)

;; Terminal
;;; Terminal shortcut
(defun my-term () (interactive) (ansi-term (substitute-env-vars "$SHELL")))
(global-set-key (kbd "C-c C-<return>") 'my-term)

;;; VTerm
(use-package vterm
  :ensure t)

;;; Kill buffer when inferior shell exits
;;; From: https://stackoverflow.com/a/23691628
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))
 
;;; Shell management
(use-package shell-switcher
  :ensure t
  :custom
  (shell-switcher-mode t)
  (shell-switcher-new-shell-function 'my-term))

;;; Hide line numbers
(add-hook 'term-mode-hook
		(lambda () (linum-mode 0)))
(setq term-suppress-hard-newline t)

;; VTerm
;;; Trying out to see how it is
(use-package vterm
  :ensure t)

(setq vterm-minibuffer-sess-id 10)
(setq vterm-minibuffer-name "*vterm-minibuffer*")
(defun vterm-minibuffer ()
  "Run a command specified in a minibuffer using vterm"
  (interactive)
  (setq cmd (read-from-minibuffer (format "%s $ " default-directory)))
  (let ((current-prefix-arg 10)
	   (vterm-buffer-name vterm-minibuffer-name))
    (call-interactively 'vterm))
  (vterm-send-C-u) ; Clear line, in case cmd partially entered
  (comint-send-string
   (format "%s<%d>" vterm-minibuffer-name vterm-minibuffer-sess-id)
   (format "%s\n" cmd))
  )

(define-key global-map (kbd "M-!") (lambda () (interactive) (vterm-minibuffer)))
  
;; Git integration
(use-package magit
  :ensure t)

;; Diredq
(use-package dired-avfs
  :ensure t)
(use-package dired-subtree
  :ensure t)

;; Language server protocol support
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

;; Helm
(use-package helm
  :ensure t
  :bind ("M-x" . helm-M-x)
  :bind ("C-x C-f" . helm-find-files)
  :bind ("C-z" . helm-execute-persistent-action)
  :config (helm-mode 1)
  :custom (completion-styles '(flex)))

(use-package helm-swoop
  :ensure t)
  ;; :bind ("C-s" . helm-swoop))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

;; Writeable grep buffer
(use-package wgrep
  :ensure t
  :config (use-package wgrep-helm :ensure t))

;; Rest client
(use-package restclient
  :ensure t)

								; Programming Languages

;; YAML
(use-package yaml-mode
  :ensure t)

;;Salt state
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; Markdown
(use-package markdown-mode
  :ensure t)
(setq markdown-command "/bin/pandoc")

;; Go
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (add-hook 'before-save-hook #'gofmt-before-save))

;; LaTeX
;;; Preview Latex inline
(use-package px
  :ensure t)

;; Web development
(use-package typescript-mode
  :ensure t)
(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (eldoc-mode +1)   ; Show function documentation on hover
  (company-mode +1) ; Auto complete
  (tide-setup))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  :hook (web-mode .
			   (lambda ()
				(when (string-equal "tsx" (file-name-extension buffer-file-name))
				  (setup-tide-mode))))
  :custom
  (web-mode-enable-auto-quoting nil) ; Disable auto-quoting
  )

;; Git config files
(use-package gitattributes-mode
  :ensure t)
(use-package gitconfig-mode
  :ensure t)
(use-package gitignore-mode
  :ensure t)

;; Foreman Procfile
(add-to-list 'auto-mode-alist '("\\Procfile\\'" . conf-mode))

;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

								; Key Bindings

;; Make M-f to move to the beginning of the next word
(require 'misc)
(define-key global-map (kbd "M-f") (lambda ()
				     (interactive)
				     (forward-to-word 1)))

;; Make M-F move to end of next word
(define-key global-map (kbd "M-F") (lambda ()
				     (interactive)
				     (forward-word)))

;; Org mode agenda
(define-key org-mode-map (kbd "C-c a") 'org-agenda)

								; Customize
;; Place customize files in seperate file
;; Intentionally last so that use-package can install anything required by these customizations.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

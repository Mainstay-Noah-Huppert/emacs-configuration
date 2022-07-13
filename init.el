								; Emacs Core
;; Pop up debug information on error
;; (setq debug-on-error t)

(message "---------- NEW init.el LOAD ----------")

;; Local plugins
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Machine specific files - before
(setq machine-specific-file
	 (concat
	  (expand-file-name "~/.emacs.d/machines/")
	  (concat (system-name) ".el")))
(if (file-exists-p machine-specific-file) 
    (load machine-specific-file))

;; Secret values
(if (file-exists-p "~/.emacs.d/secret.el")
    (load "~/.emacs.d/secret.el"))

;; Built in package manager
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
(defcustom my-dark-or-light 'dark
  "Indicates whether the dark or light theme should be used. Valid values

  `light'
  `dark'")
(setq my-theme (if (and (boundp 'my-dark-or-light) (eq my-dark-or-light 'light)) 'doom-one-light 'doom-one))
(use-package doom-themes
  :ensure t
  :config
  (load-theme my-theme t))
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
	 '((sequence "TODO" "DOING" "PAUSED" "BLOCKED" "|" "DONE" "ABANDONED")))

(setq org-tempo-keywords-alist
	 '(("L" . "latex")
	  ("H" . "html")
	  ("A" . "ascii")
	  ("i" . "index")
	  ("s" . "src")))
(setq org-startup-with-inline-images t) ; Make show inline images

(use-package ob-async
  :ensure t
  :init (setq org-ctrl-c-ctrl-c 'ob-async-org-babel-execute-src-block))
(use-package ob-restclient
  :ensure t
  :config (org-babel-do-load-languages
		 'org-babel-load-languages
		 '((restclient . t)
		   (dot . t)
		   (sql . t))))
(defun my-org-babel-confirm-evaluate (lang body)
  "Checks to see if the babel code block contains the :trust argument in the header.
If it does confirmation is not required to run the code block."
  (let ((info (org-babel-get-src-block-info)))
    (if info
	   (if (assoc :trust (nth 2 info))
		  nil
		t)
	 t)
    )
  )
(setq org-confirm-babel-evaluate #'my-org-babel-confirm-evaluate)

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

;;; Open file on remote
(use-package browse-at-remote
  :ensure t)

;; Organize buffers by project
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :bind (:map projectile-mode-map
		    ("s-p" . 'projectile-command-map)
		    ("C-c p" . 'projectile-command-map)))

;; Make Dired show human units
(setq dired-listing-switches "-alh")

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

(load (expand-file-name "~/.emacs.d/vterm-minibuffer.el"))
(define-key global-map (kbd "M-!") 'vterm-minibuffer)
  
;; Git integration
(use-package magit
  :ensure t)

(use-package git-link
  :ensure t)

;; Dired
(use-package dired-avfs
  :ensure t)
(use-package dired-subtree
  :ensure t)
(setq dired-listing-switches (concat dired-listing-switches "h"))

;; Language server protocol support
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq web-mode-enable-auto-indentation nil)
  :hook ((web-mode . lsp))
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

(use-package helm-ag
  :ensure t
  :custom (helm-follow-mode-persistent t))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package helm-tramp
  :ensure t)

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

;; Terraform
(use-package terraform-mode
  :ensure t)

;; Web development
(use-package typescript-mode
  :ensure t)
;; (use-package tide
;;   :ensure t)

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (eldoc-mode +1)   ; Show function documentation on hover
;;   (company-mode +1) ; Auto complete
;;   (tide-setup))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    ;; :hook (web-mode .
  ;; 			   (lambda ()
  ;; 				(when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;; 				  (setup-tide-mode))))
  :custom
  (web-mode-enable-auto-quoting nil) ; Disable auto-quoting
  )

;; Graphviz dot mode
(use-package graphviz-dot-mode
  :ensure t)

;; Git config files
(use-package git-modes
  :ensure t)

;; Foreman Procfile
(add-to-list 'auto-mode-alist '("\\Procfile\\'" . conf-mode))

;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; JSON
(use-package json-mode
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t)

;; MongoDB shell
(use-package inf-mongo
  :ensure t)

;; GraphQL
(use-package graphql-mode
  :ensure t)


;; Terraform
(use-package terraform-mode
  :ensure t)

;; Godot
(use-package gdscript-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package graphql-mode
  :ensure t)
(use-package request
  :ensure t)
;; JSON
(use-package json-mode
  :ensure t)

;; Haskell
(use-package haskell-mode
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
    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rust-mode yaml-mode wgrep-helm web-mode vterm use-package typescript-mode transpose-frame terraform-mode shell-switcher rainbow-mode px org-bullets ob-restclient ob-async magit lsp-python-ms json-mode inf-mongo helm-tramp helm-swoop helm-projectile helm-lsp helm-ag haskell-mode graphviz-dot-mode graphql-mode go-mode gitignore-mode gitconfig-mode gitattributes-mode gdscript-mode flycheck editorconfig doom-themes dockerfile-mode dired-subtree dired-avfs dash-functional browse-at-remote)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Machine specific files - after
(setq machine-specific-file
	 (concat
	  (expand-file-name "~/.emacs.d/machines/")
	  (concat (system-name) "-post.el")))
(if (file-exists-p machine-specific-file) 
    (load machine-specific-file))

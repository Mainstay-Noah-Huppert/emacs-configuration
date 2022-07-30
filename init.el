								; Emacs Core
;; Pop up debug information on error
;; (setq debug-on-error t)
;; (toggle-debug-on-quit 1)
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

<<<<<<< HEAD
(load (expand-file-name "~/.emacs.d/vterm-minibuffer.el"))
=======
;; VTerm
;;; Trying out to see how it is
(use-package vterm
  :ensure t)

;;; TODO: history
;;; TODO: helm auto complete
;;; TODO: Better window handlings
;;;       - Make easy to close vterm?
;;;       - Customize if output is focused (default: don't??)
;;; TODO: Cancel buffer & window focus on canceled cmd query input
;;; TODO: Open vterm window and focus without entering command
(setq vterm-minibuffer-sess-id 10)
(setq vterm-minibuffer-name-base "*vterm-minibuffer*")

(defun vterm-minibuffer-base-dir-try-vc (buffer)
  "Attempts to return the version control project root directory, if this fails uses the directory associated with the open buffer, if this fails uses the home directory, if this fails uses default-directory."
  (interactive)
  (or (vc-root-dir)
	 (buffer-file-name)
	 (expand-file-name "~/")
	 (default-directory)))

(setq vterm-minibuffer-base-dir-function 'vterm-minibuffer-base-dir-try-vc)
(defun vterm-minibuffer-base-dir (buffer)
  "Returns the base directory in which shells will be opened. This is set by the variable vterm-minibuffer-base-dir-function."
  (interactive)
  (funcall vterm-minibuffer-base-dir-function buffer))

(defun vterm-minibuffer-vterm-buffer-name (base-dir)
  " Returns the name of a VTerm buffer for the specified base-dir."
  (format "%s [%s]"
		vterm-minibuffer-name-base
		base-dir))

(defun vterm-minibuffer-vterm-buffer (base-dir id)
  "Makes a interactive VTerm buffer for the specified base-dir if one does not exist.

ID is the numerical prefix argument to pass to vterm when creating or re-using existing VTerm buffer.

Returns the new or existing VTerm buffer."
  (interactive)
  (let ((current-prefix-arg id)
	   (vterm-buffer-name (vterm-minibuffer-vterm-buffer-name base-dir))) ; Tell vterm fn the name of the new vterm buffer we want
    (call-interactively 'vterm)))

;; The function to use when vterm-minibuffer-split-window-next determines a new window needs to be created. This function should return the new window object.
(setq vterm-minibuffer-split-window-next-split-function 'split-window-below)
(defun vterm-minibuffer-split-window-next ()
  "Selects a new window for the vterm buffer to use. If there is only one window open uses the vterm-minibuffer-split-window-next-split-function to create one.
Returns window split to use for vterm."
  (interactive)
  (if (eq (next-window) (get-buffer-window)) ; If no split
	 (let ((split-result (funcall vterm-minibuffer-split-window-next-split-function)))
	   (select-window split-result)
	   split-result)
    (let ((next-window-v (next-window)))
	 (select-window next-window-v)
	 next-window-v)))

(defun vterm-minibuffer-split-window-other ()
  " Selects the other window regardless of if it is the same as the origin window.
Returns this window."
  (interactive)
    (select-window (next-window))
    (get-buffer-window))

;; The function vterm-minibuffer-split-window should use to select the window for the vterm buffer. This function should select and return the window.
(setq vterm-minibuffer-split-function 'vterm-minibuffer-split-window-next)
;; (setq vterm-minibuffer-split-function 'vterm-minibuffer-split-window-other)
(defun vterm-minibuffer-split-window ()
  "Makes a window using vterm-minibuffer-split-function which will be used to house the vterm buffer."
  (interactive)
  (funcall vterm-minibuffer-split-function))

(defun vterm-minibuffer (subid)
  "Run a command specified in a minibuffer using vterm.

SUBID specifies if which of the potential multiple shells for the vterm-minibuffer-base-dir to execute the command within. The SUBID 1 is automatically created, specify SUBIDs great than 1 to make new shells."
  (interactive "p") ; Numeric prefix argument, no prompt
  (unless subid (setq subid 1))
  (let ((origin-buffer (current-buffer)))
    (let ((base-dir (vterm-minibuffer-base-dir origin-buffer)))
    (let ((origin-window (get-buffer-window))		
		(split-window (vterm-minibuffer-split-window))
		(vterm-buffer (vterm-minibuffer-vterm-buffer base-dir subid)) ; TODO: Fix bug where vterm shell doesn't actually open in correct directory!
		(cmd (read-from-minibuffer (format "<%d> %s $ " subid base-dir))))
	 (select-window origin-window)
	 ;; If switching to origin window didn't end us back at origin buffer => probably only one window open.
	 ;; Explicitly switch back to origin buffer.
	 (if (not (eq (current-buffer) origin-buffer))
		(switch-to-buffer origin-buffer))
	 (vterm-send-C-u) ; Clear cmd prompt, in case another cmd partially entered
	 (comint-send-string ; Send cmd to vterm buffer and run
	  vterm-buffer
	  (format "%s\n" cmd))))))

>>>>>>> 972cd98 (vterm-minibuffer: Fixed some bugs)
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
  (setq lsp-log-io t)
  :config
  (lsp-register-client (make-lsp-client :new-connection (lsp-tramp-connection (list "typescript-language-server" "--stdio"))
								:major-modes '(web-mode)
								:remote? t
								:priority 1
								:server-id 'typescript))
    ;; (lsp-register-client (make-lsp-client :new-connection (lsp-tramp-connection "deno")
    ;; 								:major-modes '(web-mode)
    ;; 								:remote? t
    ;; 								:priority 1
    ;; 								:initialization-options '(:enable t)
    ;; 								:server-id 'deno-ls))
  :hook ((web-mode . lsp))
  :commands lsp)
(use-package yasnippet
  :ensure t)

(use-package eglot
  :ensure t)

(use-package company
  :ensure t
  :init (global-company-mode))

;; Helmq
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

;; C#
(use-package csharp-mode
  :ensure t)

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

;; Machine specific files - after
(setq machine-specific-file
	 (concat
	  (expand-file-name "~/.emacs.d/machines/")
	  (concat (system-name) "-post.el")))
(if (file-exists-p machine-specific-file) 
    (load machine-specific-file))

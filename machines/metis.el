;; Font size within Emacs
;;; 140  - for laptop screen
;;; 200 - for 4k
(defun determine-best-font-size () (if (> (display-pixel-width) 1800) 200 140))
(setq my-font-size (determine-best-font-size))
(defun set-best-font-size () (interactive) (set-face-attribute 'default nil :height (determine-best-font-size)))

;; Set custom path locations
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin:/Library/TeX/texbin/"))
(setq ispell-program-name "/usr/local/bin/ispell")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin/")

;; Location of Org mode agenda files on this machine
(setq my-org-agenda-files
	 (list
	  "~/Documents/planner.org"
	  "~/Documents/planner.org_archive"))

;; Set Magit to use Homebrew version of Git
(setq magit-git-executable "/usr/local/bin/git")

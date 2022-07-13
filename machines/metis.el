;; Font size within Emacs
;;; 140  - for laptop screen
;;; 200 - for 4k
(setq my-font-size-laptop 140)
(setq my-font-size-4k 200)
(defun determine-best-font-size () (if (> (display-pixel-width) 1800) my-font-size-4k my-font-size-laptop))
(setq my-font-size (determine-best-font-size))
(defun set-best-font-size () (interactive) (set-face-attribute 'default nil :height (determine-best-font-size)))

(defun set-font-size (size) (interactive) (set-face-attribute 'default nil :height size))

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

;; Org mode LaTeX exports
(setq org-latex-listings 'minted
	 org-latex-packages-alist '(("" "listingsutf8")
						   ("" "minted")
						   ("margin=0.5in" "geometry" nil))
	 org-latex-pdf-process
	 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
	 org-src-fontify-natively t)

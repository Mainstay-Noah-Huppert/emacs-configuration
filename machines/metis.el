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

(setq my-font-size 200)

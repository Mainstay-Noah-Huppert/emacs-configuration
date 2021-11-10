;; Set custom path locations
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin"))
(setq ispell-program-name "/usr/local/bin/ispell")
(add-to-list 'exec-path "/usr/local/bin")

;; Location of Org mode agenda files on this machine
(setq my-org-agenda-files
	 (list
	  "~/Documents/planner.org"))

(setq my-font-size 200)

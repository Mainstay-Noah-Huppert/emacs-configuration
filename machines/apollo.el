(defun determine-best-font-size () (if (> (display-pixel-width) 1280) 120 80))

;; Font size within Emacs
;;; 80  - for laptop screen
;;; 120 - for 4k
(setq my-font-size determine-best-font-size))
(defun set-best-font-size () (interactive) (set-face-attribute 'default nil :height (determine-best-font-size)))

;; Location of Org mode agenda files on this machine
(setq my-org-agenda-files
	 (list
	  "~/documents/planner.org"
	  "~/documents/planner.org_archive"
	  "~/documents/school/y5/afroam/afroam-133/planner.org"
	  "~/documents/school/y5/afroam/afroam-133/lectures.org"
	  "~/documents/school/y5/afroam/afroam-133/reading.org"
	  "~/documents/school/y5/cs/cs-alligator/planner.org"
	  "~/documents/school/y5/cs/cs-370/planner.org"
	  "~/documents/school/y5/spanish/spanish-240/planner.org"
	  "~/documents/school/y5/cs/cics-305/planner.org" 
	  "~/documents/school/y5/cs/cs-590a/planner.org"))

;; Tramp for coder
(tramp-set-completion-function "ssh"
						 '((tramp-parse-sconfig "~/.ssh/config")
						   (tramp-parse-sconfig "~/.ssh/coder")))

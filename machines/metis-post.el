;; Tramp for coder
(tramp-set-completion-function "ssh"
						 '((tramp-parse-sconfig "~/.ssh/config")
						   (tramp-parse-sconfig "~/.ssh/coder")))

(tramp-set-completion-function "sshx"
						 '((tramp-parse-sconfig "~/.ssh/config")
						   (tramp-parse-sconfig "~/.ssh/coder")))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 10)



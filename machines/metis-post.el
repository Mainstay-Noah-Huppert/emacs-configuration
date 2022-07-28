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

								;(setq tramp-default-method "ssh")
(setq tramp-verbose 10)

;; Need to set these variables or else ansi-term freaks out when trying to parse escape code information
(setq ange-ftp-default-user nil)
(setq ange-ftp-default-password nil)
(setq ange-ftp-generate-anonymous-password nil)

(connection-local-set-profile-variables 'remote-deno-path
								 '((tramp-remote-path . ("~/.deno/bin/" tramp-default-remote-path))))
(connection-local-set-profiles '(:machine "coder.noah") 'remote-deno-path)

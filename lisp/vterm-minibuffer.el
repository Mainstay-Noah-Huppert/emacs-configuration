;;; TODO: history
;;; TODO: helm auto complete
;;; TODO: Better window handlings
;;;       - Make easy to close vterm?
;;;       - Customize if output is focused (default: don't??)
;;; TODO: Cancel buffer & window focus on canceled cmd query input
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
	 ;; Make new split
	 (let ((split-result (funcall vterm-minibuffer-split-window-next-split-function)))
	   (select-window split-result)
	   split-result)
    ;; Else select split window
    (let ((next-window-v (next-window)))
	 (select-window next-window-v)
	 next-window-v)
    )
  )

(defun vterm-minibuffer-split-window-other ()
  " Selects the other window regardless of if it is the same as the origin window.
Returns this window."
  (interactive)
    (select-window (next-window))
    (get-buffer-window))

;; The function vterm-minibuffer-split-window should use to select the window for the vterm buffer. This function should select and return the window.
;; (setq vterm-minibuffer-split-function 'vterm-minibuffer-split-window-next)
(setq vterm-minibuffer-split-function 'vterm-minibuffer-split-window-next)
(defun vterm-minibuffer-split-window ()
  "Makes a window using vterm-minibuffer-split-function which will be used to house the vterm buffer."
  (interactive)
  (funcall vterm-minibuffer-split-function))

(defun vterm-minibuffer (subid)
  "Run a command specified in a minibuffer using vterm.

SUBID specifies if which of the potential multiple shells for the vterm-minibuffer-base-dir to execute the command within. The SUBID 1 is automatically created, specify SUBIDs great than 1 to make new shells."
  (interactive "p") ; Numeric prefix argument, no prompt
  (unless subid (setq subid 1))
  (let ((origin-window (get-buffer-window))
	   (origin-buffer (current-buffer))
	   )
    (let ((base-dir (vterm-minibuffer-base-dir origin-buffer))
		)
    (let ((split-window (vterm-minibuffer-split-window))
		(vterm-buffer (vterm-minibuffer-vterm-buffer base-dir subid))
		(cmd (read-from-minibuffer (format "<%d> %s $ " subid base-dir)))
		)
	 (select-window origin-window)
	 ;; If switching to origin window didn't end us back at origin buffer => probably only one window open.
	 ;; Explicitly switch back to origin buffer.
	 (if (not (eq (current-buffer) origin-buffer))
		(switch-to-buffer origin-buffer))
	 (vterm-send-C-u) ; Clear cmd prompt, in case another cmd partially entered
	 (comint-send-string ; Send cmd to vterm buffer and run
	  vterm-buffer
	  (format "%s\n" cmd))
	 )
    )
    )
  )

;;; TODO: history
;;; TODO: helm auto complete
;;; TODO: Better window handlings
;;;       - Make easy to close vterm?
;;;       - Customize if output is focused (default: don't??)
;;; TODO: Cancel buffer & window focus on canceled cmd query input
(defcustom vterm-minibuffer-name-base "*vterm-minibuffer*"
  "Name of VTerm buffers which appears first (before directory or subid info).")

(defun vterm-minibuffer-base-dir-try-vc (buffer)
  "Attempts to return the version control project root directory, if this fails uses the directory associated with the open buffer, if this fails uses the home directory, if this fails uses `default-directory'."
  (interactive)
  (or (vc-root-dir)
	 (buffer-file-name)
	 (expand-file-name "~/")
	 (default-directory)))

(defcustom vterm-minibuffer-base-dir-function 'vterm-minibuffer-base-dir-try-vc
  "Function used to determine the base directory in which to open a VTerm.")
(defun vterm-minibuffer-base-dir (buffer)
  "Returns the base directory in which shells will be opened. This is set by the variable `vterm-minibuffer-base-dir-function'."
  (interactive)
  (funcall vterm-minibuffer-base-dir-function buffer))

(defun vterm-minibuffer-vterm-buffer-name (base-dir)
  " Returns the name of a VTerm buffer for the specified BASE-DIR."
  (format "%s [%s]"
		vterm-minibuffer-name-base
		base-dir))

(defun vterm-minibuffer-vterm-buffer (base-dir id)
  "Makes a interactive VTerm buffer for the specified BASE-DIR if one does not exist.

ID is the numerical prefix argument to pass to vterm when creating or re-using existing VTerm buffer.

Returns the new or existing VTerm buffer."
  (interactive)
  (let ((current-prefix-arg id)
	   (vterm-buffer-name (vterm-minibuffer-vterm-buffer-name base-dir))) ; Tell vterm fn the name of the new vterm buffer we want
    (call-interactively 'vterm)))

(defcustom vterm-minibuffer-split-window-next-split-function 'split-window-below
    "The function to use when `vterm-minibuffer-split-window-next' determines a new window needs to be created. This function should return the new window object.")
(defun vterm-minibuffer-split-window-next ()
  "Selects a new window for the vterm buffer to use. If there is only one window open uses the `vterm-minibuffer-split-window-next-split-function' to create one.
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

(defcustom vterm-minibuffer-split-function 'vterm-minibuffer-split-window-other
  "The function `vterm-minibuffer-split-window' should use to select the window for the vterm buffer. This function should select and return the window.

`vterm-minibuffer-split-window-next' is an alternative value.")
(defun vterm-minibuffer-split-window ()
  "Makes a window using `vterm-minibuffer-split-function' which will be used to house the vterm buffer."
  (interactive)
  (funcall vterm-minibuffer-split-function))

(defcustom vterm-minibuffer-single-window-return-to-origin-buffer t
  "If non-nil indicates that `vterm-minibuffer' should always try to return back to the buffer which was selected when it was called, even if there only ends up being one window open.

For example if `vterm-minibuffer-split-function' does not create a new window, and there is only one window open, then the origin buffer and the VTerm buffer can only be displayed within the same window. This means if the origin buffer is to be displayed the VTerm buffer will end up being hidden. If this variable is nil then `vterm-minibuffer' will not try to switch back to the origin buffer if it means the VTerm buffer would be hidden.")
(defun vterm-minibuffer (subid)
  "Run a command specified in a minibuffer using vterm.

SUBID specifies which of the potential multiple shells for the `vterm-minibuffer-base-dir' to execute the command within. The SUBID 1 is automatically created, specify SUBIDs great than 1 to make new shells.

The behavior of this function can be tweaked by:
`vterm-minibuffer-split-function' - Customize how windows are split to display the VTerm buffer
`vterm-minibuffer-single-window-return-to-origin-buffer' - Cuztomize behavior of switching back to origin buffer"
  (interactive "p") ; Numeric prefix argument, no prompt
  (unless subid (setq subid 1))
  (let ((origin-window (get-buffer-window))
	   (origin-buffer (current-buffer)))
    (let ((base-dir (vterm-minibuffer-base-dir origin-buffer)))
    (let ((split-window (vterm-minibuffer-split-window))
		(vterm-buffer (vterm-minibuffer-vterm-buffer base-dir subid)) ; Create or retrieve existing VTerm buffer
		(cmd (read-from-minibuffer (format "<%d> %s $ " subid base-dir)))) ; Prompt cmd to run
	 (select-window origin-window)
	 ;; If switching to origin window didn't end us back at origin buffer => probably only one window open.
	 ;; Explicitly switch back to origin buffer.
	 (if (and vterm-minibuffer-single-window-return-to-origin-buffer (not (eq (current-buffer) origin-buffer)))
		(switch-to-buffer origin-buffer))
	 (vterm-send-C-u) ; Clear cmd prompt, in case another cmd partially entered
	 (comint-send-string ; Send cmd to vterm buffer and run
	  vterm-buffer
	  (format "%s\n" cmd))
	 )
    )
    )
  )

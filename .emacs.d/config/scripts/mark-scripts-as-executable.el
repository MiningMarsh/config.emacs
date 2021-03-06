;;; mark-scripts-as-executable ---  If a file is saved that has a shebang magic number, mark it as executable.
;;; Commentary:
;;; Code:
(add-hooks (after-save)
	   (and
	    (buffer-is-script)
	    (not (file-executable-p buffer-file-name))
	    (set-file-modes
	     buffer-file-name
	     (logior (file-modes buffer-file-name) #o100))
	    (message (concat "Marked executable: " buffer-file-name))))
;;; mark-scripts-as-executable.el ends here

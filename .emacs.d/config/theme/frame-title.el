;;; frame-title --- Set the frame title to the current file path.
;;; Commentary:
;;; Code:

(setq frame-title-format
      ;; I stole this from some guy online, what the fuck is this shit? Who the
      ;; hell manually invokes eval? What the hell is wrong with emacs?
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
;;; frame-title.el ends here

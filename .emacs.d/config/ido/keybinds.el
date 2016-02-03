;;; keybinds --- flx-ido keybindings.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (flx-ido)
	   (global-set-key
	    "\M-x"
	    (lambda ()
	      (interactive)
	      (call-interactively
	       (intern
		(ido-completing-read
		 "M-x "
		 (all-completions "" obarray 'commandp)))))))
;;; keybinds.el ends here

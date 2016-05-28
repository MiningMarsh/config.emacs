;;; keybinds --- Leader bindings for debuggers.
;;; Commentary:
;;; Code:

(packages/requires (evil evil-leader gud)
	   (evil-leader/set-key-for-mode 'gud-mode
	     "b" 'gud-break
	     "r" 'gud-run
	     "s" 'gud-step
	     "S" 'gud-stepi
	     "w" 'gud-watch))
;;; keybinds.el ends here

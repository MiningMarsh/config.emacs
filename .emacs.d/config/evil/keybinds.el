;;; keybinds --- Set default evil bindigs.
;;; Commentary:
;;; This includes global evil keybindings.
;;; Code:
(eval-when-compile (require 'key-tree))
(require 'rc)

(requiring (evil evil-leader key-chord mu4e ranger exwm key-tree)

	   ;; Free up space and ret in normal mode.
	   (dolist (key (list (kbd "RET") " "))
	     (move-key
	      evil-motion-state-map evil-normal-state-map
	      key))

	   (key-tree/add-bindings
	    "f" ("File"
		 "f" "Find File" 'find-file
		 "F" "File Manager" 'deer)

	    "b" ("Buffer"
		 "s" ("Switch Buffer"
		      "f" "Find Buffer" 'switch-to-buffer
		      "m" "Switch to *Messages*" (interactively
						  (switch-to-buffer
						   "*Messages*"))
		      "s" "Switch to *scratch*" (interactively
						 (switch-to-buffer
						  "*scratch*"))
		      "b" "Switch to *Backtrace*" (interactively
						   (switch-to-buffer
						    "*Backtrace*"))
		      "c" "Switch to *Compile-Log*" (interactively
						     (switch-to-buffer
						      "*Compile-Log*")))
		 "k" ("Kill Buffer"
		      "c" "Kill Current Buffer" 'kill-this-buffer))
	    "w" ("Window"
		 "S" ("Split Window"
		      "h" "Split Window Horizontally" 'split-window-below
		      "v" "Split Window Vertically" 'split-window-right)
		 "d" ("Delete Window"
		      "c" "Delete Current Window" 'delete-window
		      "o" "Delete Other Windows" 'delete-other-windows)
		 "s" ("Switch Window"
		      "c" "Cycle Window" 'other-window))

	    "p" ("Process"
		 "l" ("Launch Process"
		      "f" "Launch Firefox" (interactively (launch-program "firefox"))
		      "l" "Launch Libreoffice" (interactively (launch-program "libreoffice"))
		      "e" "Launch ERC" 'erc-tls))))
;;; keybinds.el ends here

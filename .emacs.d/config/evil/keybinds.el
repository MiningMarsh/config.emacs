;;; keybinds --- Set default evil bindigs.
;;; Commentary:
;;; This includes global evil keybindings.
;;; Code:
(eval-when-compile (require 'key-tree))

(packages/requires (evil evil-leader key-chord ranger key-tree os mac)

		   ;; Free up space and ret in normal mode.
		   (dolist (key (list (kbd "RET") " "))
		     (move-key
		      evil-motion-state-map evil-normal-state-map
		      key))

		   (key-tree/add-bindings
		    e ("Emacs"
		       c "Run Command" 'smex
		       b "Bind Symbol" 'bind-symbol
		       s "Get Symbol" 'retrieve-symbol)

		    f ("File"
		       f "Find File" 'find-file
		       F "File Manager" 'deer
		       c "Change Directory" 'cd)

		    b ("Buffer"
		       s ("Switch Buffer"
			  f "Find Buffer" 'switch-to-buffer
			  m "Switch to *Messages*" (interactively
						    (switch-to-buffer
						     "*Messages*"))
			  s "Switch to *scratch*" (interactively
						   (switch-to-buffer
						    "*scratch*"))
			  b "Switch to *Backtrace*" (interactively
						     (switch-to-buffer
						      "*Backtrace*"))
			  c "Switch to *Compile-Log*" (interactively
						       (switch-to-buffer
							"*Compile-Log*")))
		       k ("Kill Buffer"
			  c "Kill Current Buffer" 'kill-this-buffer
			  f "Find and Kill Buffer" 'ido-kill-buffer))

		    w ("Window"
		       S ("Split Window"
			  "h" "Split Window Horizontally" 'split-window-below
			  "v" "Split Window Vertically" 'split-window-right)
		       d ("Delete Window"
			  "c" "Delete Current Window" 'delete-window
			  "o" "Delete Other Windows" 'delete-other-windows)
		       s ("Switch Window"
			  "c" "Cycle Window" 'other-window))

		    P ("Process"
		       l ("Launch Process"
			  f "Launch Firefox" (interactively
					      (launch-program "firefox"))
			  l "Launch Libreoffice" (interactively
						  (launch-program
						   "libreoffice"))
			  e "Launch ERC" 'erc-tls))

		    h ("Help"
		       K "Verbose Key Sequence Binding" 'describe-key
		       f "Function" 'describe-function
		       k "Key Sequence Binding" 'describe-key-briefly
		       m "Mode" 'describe-mode
		       p "Position" 'describe-char
		       v "Variable" 'describe-variable)

		    F ("Feature"
		       i "Install" 'package-install
		       u "Upgrade" 'packages/upgrade
		       I "Install and Upgrade" 'packages/install-or-upgrade-if-needed
		       r "Remove" 'packages/uninstall
		       R "Remove Outdated Versions" 'packages/uninstall-outdated
		       l "Load" (lambda (package)
				  (-> (completing-read "Load Feature: "
						       features
						       (lambda (&rest args) t)
						       t)
				      intern-soft
				      list
				      interactive)
				  (require package))))

		   (os/when-mac
		    (key-tree/add-bindings
		     o ("Operating System"
			a ("Account"
			   "l" "Lock" (interactively (mac/lock-account))
			   "L" "Log Out" (interactively (mac/log-out)))

			p ("Power"
			   "s" "Sleep" (interactively (mac/sleep))
			   "S" "Shutdown" (interactively (mac/shutdown))
			   "r" "Restart" (interactively (mac/restart)))))))
;;; keybinds.el ends here

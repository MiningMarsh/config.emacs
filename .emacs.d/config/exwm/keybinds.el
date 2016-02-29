;;; keybinds --- EXWM keybindings.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (exwm)

	   (exwm-input-set-key
	    (kbd "<XF86AudioRaiseVolume>")
	    (lambda ()
	      (interactive)
	      (! "amixer set Master 5%+ | tail -n2 | sed -E 's;^\s+;;g'")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioLowerVolume>")
	    (lambda ()
	      (interactive)
	      (! "amixer set Master 5%- | tail -n2 | sed -E 's;^\s+;;g'")))

	   (exwm-input-set-key
	    (kbd "<XF86MonBrightnessDown>")
	    (lambda ()
	      (interactive)
	      (shell-command-to-string "xbacklight -dec 25")))

	   (exwm-input-set-key
	    (kbd "<XF86MonBrightnessUp>")
	    (lambda ()
	      (interactive)
	      (shell-command-to-string "xbacklight -inc 25")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioStop>")
	    (lambda ()
	      (interactive)
	      (! "mpc stop")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioNext>")
	    (lambda ()
	      (interactive)
	      (! "mpc next")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioPrev>")
	    (lambda ()
	      (interactive)
	      (! "mpc prev")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioPlay>")
	    (lambda ()
	      (interactive)
	      (! "mpc toggle | head -n2")))

	   (exwm-input-set-key
	    (kbd "<pause>")
	    (lambda ()
	      (interactive)
	      (! "mpc toggle | head -n2")))

	   (exwm-input-set-key
	    (kbd "<XF86AudioMute>")
	    (lambda ()
	      (interactive)
	      (! "amixer set Master toggle | tail -n2 | sed -E 's;^\s+;;g'")))

	   (exwm-input-set-key
	    (kbd "s-<end>")
	    (lambda ()
	      (interactive)
	      (with-temp-message "Ejecting removable devices..."
		(shell-command-to-string "devmon -r"))))

	   (exwm-input-set-key
	    (kbd "s-<return>")
	    (lambda ()
	      (interactive)
	      (launch-program "urxvtc")))

	   ;; Set prefix key for launching programs.
	   (add-to-list 'exwm-input-prefix-keys ?\s-p)

	   ;; Set a new keybind for sending literal keys.
	   (exwm-input-set-key
		(kbd "s-\\")
		'exwm-input-send-next-key)

	   ;; Set up some keys for copy and paste.
	   (exwm-input-set-simulation-keys
		(assoc-map [?\s-c] 3 ;; C-c
				   [?\s-v] 22)) ;; C-v

	   ;; Bind keys to launch programs.
	   (mapc (lambda (atom)
		   (lexical-let ((atom atom))
		     (exwm-input-set-key
		      (kbd (format "s-p %s" (car atom)))
		      (lambda ()
			(interactive)
			(launch-program (cdr atom))))))
		 (assoc-map "f" "firefox"
			    "l" "libreoffice"
			    "u" "urxvtc"))

	   (requiring (buffer-move)
		      (mapc (lambda (atom)
			      (exwm-input-set-key (car atom) (cdr atom)))
			    (assoc-map
			     (kbd "s-SPC") #'tiling-cycle
			     (kbd "s-k") #'windmove-up
			     (kbd "s-j") #'windmove-down
			     (kbd "s-h") #'windmove-left
			     (kbd "s-l") #'windmove-right
			     (kbd "s-K") #'buf-move-up
			     (kbd "s-J") #'buf-move-down
			     (kbd "s-H") #'buf-move-left
			     (kbd "s-L") #'buf-move-right))))
;;; keybinds.el ends here

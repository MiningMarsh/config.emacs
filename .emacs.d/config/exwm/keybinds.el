;;; keybinds --- EXWM keybindings.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (exwm window-number nyan-mode)

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

	   (lexical-let ((playing nil)
			 (last-fired (second (current-time))))
	     (exwm-input-set-key
	      (kbd "s-M")
	      (lambda ()
		(interactive)
		(unless (= last-fired (second (current-time)))
		  (setq last-fired (second (current-time)))
		  (setq playing (not playing))
		  (if playing
		      (progn
			(message "Beggining freedom chant.")
			(start-music))
		    (progn
		      (message "Stay free, hacker.")
		      (stop-music)))))))

	   ;; Set a new keybind for sending literal keys.
	   (exwm-input-set-key
	    (kbd "s-\\")
	    'exwm-input-send-next-key)

	   ;; Set up some keys for copy and paste.
	   (exwm-input-set-simulation-keys
	    (assoc-map [?\s-c] 3 ;; C-c
		       [?\s-v] 22)) ;; C-v

	   ;; Set up some window focus/movement keys.
	   (let1 binds (zip (list "q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
			    (number-sequence 1 10))
		 (dolist (bind binds)

		   ;; Make the binding lexical for the following lambdas.
		   (lexical-let ((bind bind))

		     ;; Focus windows.
		     (exwm-input-set-key
		      (kbd (format "s-%s" (car bind)))
		      (lambda ()
			(interactive)
			(window-number-select (cadr bind))))

		     ;; Swap windows.
		     (exwm-input-set-key
		      (kbd (format "s-%s" (upcase (car bind))))
		      (lambda ()
			(interactive)
			(let1 selected (selected-window)
			      (window-number-select (cadr bind))
			      (swap-buffers selected (selected-window)))))

		     ;; Close windows.
		     (exwm-input-set-key
		      (kbd (format "C-s-%s" (car bind)))
		      (lambda ()
			(interactive)

			;; Record the currently selected window.
			(let ((selected (selected-window))
			      (the-same nil))

			  ;; Select the new window.
			  (window-number-select (cadr bind))

			  ;; Record whether the window was the same as what it was before.
			  (when (equal (selected-window) selected)
			    (setq the-same t))

			  ;; Delete the target window.
			  (delete-window (selected-window))

			  ;; If needed, restore focus to the original window.
			  (when (not the-same)
			    (select-window selected)))))

		     ;; Move windows.
		     )))

	   ;; Set prefix key for launching programs.
	   (add-to-list 'exwm-input-prefix-keys ?\s-z)

	   ;; Bind keys to launch programs.
	   (mapc (lambda (atom)
		   (lexical-let ((atom atom))
		     (exwm-input-set-key
		      (kbd (format "s-z %s" (car atom)))
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
			     (kbd "s-k") #'windmove-up
			     (kbd "s-j") #'windmove-down
			     (kbd "s-h") #'windmove-left
			     (kbd "s-l") #'windmove-right
			     (kbd "s-K") #'buf-move-up
			     (kbd "s-J") #'buf-move-down
			     (kbd "s-H") #'buf-move-left
			     (kbd "s-L") #'buf-move-right))))
;;; keybinds.el ends here

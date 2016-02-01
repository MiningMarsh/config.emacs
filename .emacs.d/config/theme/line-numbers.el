;;; line-numbers --- Enable line numbers.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (relative-line-numbers linum)
	   ;; Set initial line numbering color.
	   (custom-set-faces
	    '(linum ((t (:inherit (shadow default) :foreground "yellow")))))

	   (add-hooks (evil-motion-state-entry-hook
		       evil-operator-state-entry-hook)
		      ;; We want relative line numbering.
		      (relative-line-numbers-mode 1)
		      line-number-mode
		      ;; Make line numbers orange.
		      (custom-set-faces
		       '(linum ((t (:inherit (shadow default) :foreground "orange")))))
		      column-number-mode)
	   (add-hooks (evil-motion-state-exit-hook
		       evil-operator-state-exit-hook)
		      ;; Reset the line color when exitting.
		      (custom-set-faces
		       '(linum ((t (:inherit (shadow default) :foreground "yellow")))))
		      (relative-line-numbers-mode -1))


	   ;; Absolute line numbering when in normal mode.
	   (add-hooks (evil-normal-state-entry-hook)
		      (linum-mode 1)
		      line-number-mode
		      column-number-mode)
	   (add-hooks (evil-normal-state-exit-hook)
		      (linum-mode -1))

	   ;; Format to make line numbers look a bit nicer.
	   (setq line-number-format-string "%4d\u2502")
	   
	   (defun generate-line-number-format-string ()
		 "Generates a line number format string for the current buffer."
		 (->> (lines-in-buffer)
			  (format "%d")
			  length
			  (format "%%%dd\u2502")))

	   ;; Set line number color.
	   (custom-set-faces
	    '(linum ((t (:inherit (shadow default) :foreground "yellow")))))

	   ;; Custom var for toggling line number mode.
	   ;; TODO: Fix this, bind it to <SPC> <SPC> in normal mode.
	   ;;       Can probably get rid relative-line-numbers-mode and use just
	   ;;       a custom format function for linum mode (see below, in
	   ;;       progress).
	   (defvar line-numbers-are-absolute nil
	     "Whether relative-line-numbers should be rendered absolutlely.")

	   ;; Set line numbers to use said format.
	   (setq linum-format line-number-format-string)
	   (setq relative-line-numbers-format
		 (lambda (offset)
		   (if (or line-numbers-are-absolute (= 0 offset))
		       (->> (current-line-number)
			    (+ offset)
			    (format (generate-line-number-format-string)))
		     (format (generate-line-number-format-string) (abs offset))))))
;;; line-numbers.el ends here

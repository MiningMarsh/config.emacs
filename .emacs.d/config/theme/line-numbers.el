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

	   ;; Set line number color.
	   (custom-set-faces
	    '(linum ((t (:inherit (shadow default) :foreground "yellow")))))

	   ;; Set line numbers to use said format.
	   (setq linum-format line-number-format-string)
	   (setq relative-line-numbers-format
		 (lambda (offset)
		   (if (= 0 offset)
		       (format line-number-format-string (current-line-number))
		     (format line-number-format-string (abs offset))))))
;;; line-numbers.el ends here

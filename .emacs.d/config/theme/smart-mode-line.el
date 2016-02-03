;;; smart-mode-line --- Enable smart mode line globally.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (smart-mode-line nyan-mode)
	   ;; Technically this is "wrong", "disgusting", and "literally hitler",
	   ;; but I prefer this to fucking with custom (which always adds to
	   ;; init.el).
	   (setq sml/no-confirm-load-theme t)

	   ;; Dirty fix.
	   (let1 nyan-enable nil
		 (when nyan-mode
		   (setq nyan-enable t)
		   (nyan-mode -1))
		 (sml/setup)
		 (when nyan-enable
		   (nyan-mode 1))))
;;; smart-mode-line.el ends here

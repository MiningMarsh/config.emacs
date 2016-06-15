;;; flx-ido --- Enable flx-ido fuzzy matching.
;;; Commentary:
;;; Code:

(packages/requires (flx-ido)
		   (flx-ido-mode 1)
		   ;; Disable ido faces to see flx highlights.
		   (setq ido-enable-flex-matching t)
		   (setq ido-use-faces nil))
;;; flx-ido.el ends here

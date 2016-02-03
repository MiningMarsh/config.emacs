;;; flx-ido --- Enable flx-ido fuzzy matching.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (flx-ido)
	   (ido-mode 1)
	   (ido-everywhere 1)
	   (flx-ido-mode 1)
	   ;; disable ido faces to see flx highlights.
	   (setq ido-enable-flex-matching t)
	   (setq ido-use-faces nil))
;;; flx-ido.el ends here

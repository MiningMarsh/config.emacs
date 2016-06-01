;;; when-to-enable-evil --- Enable EVIL.
;;; Commentary:
;;; Currently enables evil globally.  This needs to be fixed.
;;; Code:
(packages/requires (evil evil-leader)
		   (global-evil-leader-mode 1)
		   (evil-mode 1))
;;; when-to-enable-evil.el ends here

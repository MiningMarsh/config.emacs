;;; surpress-mode-messages --- Remove evil messages printed when changing mode.
;;; Commentary:
;;; Spacemacs theme has a mode marker, so no need for the extra messages.
;;; Code:
(packages/requires (evil)
		   (setq evil-insert-state-message nil
			 evil-visual-state-message nil
			 evil-motion-state-message nil
			 evil-emacs-state-message nil
			 evil-replace-state-message nil
			 evil-operator-state-message nil))
;;; surpress-mode-messages.el ends here

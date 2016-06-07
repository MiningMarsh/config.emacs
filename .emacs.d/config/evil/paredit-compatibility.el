;;; paredit-compatibility --- Enable evil-paredit-mode when paredit is enabled.
;;; Commentary:
;;; Code:
(packages/requires (paredit evil-paredit)
	   ;; Enable evil compatibility mode.
	   (add-hooks (paredit-mode)
		      evil-paredit-mode))
;;; paredit-compatibility.el ends here

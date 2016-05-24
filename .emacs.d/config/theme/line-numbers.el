;;; line-numbers --- Enable line numbers.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (relative-line-numbers linum)

	   (add-hooks (evil-motion-state-entry-hook
		       evil-operator-state-entry-hook)
		      (linum-mode 0)
		      (relative-line-numbers-mode 1))
	   (add-hooks (evil-motion-state-exit-hook
		       evil-operator-state-exit-hook)
		      (linum-mode 1)
		      (relative-line-numbers-mode -1))

	   (global-linum-mode 1)
	   (relative-line-numbers-mode 0))
;;; line-numbers.el ends here

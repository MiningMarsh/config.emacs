;;; line-numbers --- Enable line numbers.
;;; Commentary:
;;; Code:
(packages/requires (relative-line-numbers linum)

  (defvar evil/line-numbers t)

  (add-hooks (evil-motion-state-entry
    evil-operator-state-entry)
    (linum-mode 0)
    (relative-line-numbers-mode (if evil/line-numbers 1 -1)))

  (add-hooks (evil-motion-state-exit
      evil-operator-state-exit)
      (linum-mode (if evil/line-numbers 1 0))
      (relative-line-numbers-mode -1))

  (global-linum-mode 1)
  (relative-line-numbers-mode 0))
;;; line-numbers.el ends here

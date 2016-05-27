;;; winner --- Enable winner mode.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

;; Check for Xemacs.
(when (fboundp 'winner-mode)
  (winner-mode 1))
;;; winner.el ends here

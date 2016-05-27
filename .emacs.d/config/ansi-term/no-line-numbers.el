;;; no-line-numbers --- Disable line numbers in ansi-term.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(advice-add
 'ansi-term
 :after
 (lambda (&rest args)
   (line-number-mode -1)
   (linum-mode -1)))
;;; no-line-numbers.el ends here

;;; no-line-numbers --- Disable line numbers in ansi-term.
;;; Commentary:
;;; Code:
(require 'rc)

(advice-add
 'ansi-term
 :after
 (lambda (&rest args)
   (line-number-mode -1)))
;;; no-line-numbers.el ends here

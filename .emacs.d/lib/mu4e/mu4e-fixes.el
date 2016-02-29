;;; mu4e-fixes --- Custom fixes to add sanity to mu4e.
;;; Commentary:
;;; Fixes some mu4e retardation.
;;; Code:
(require 'rc)

(defeat mu4e-fixes (mu4e)

  (defun mu4e-headers-prev-unread ()
    "Mu4e developers are retarded, this fixes that"
    (mu4e-headers-next-unread t)))
;;; mu4e-fixes.el ends here

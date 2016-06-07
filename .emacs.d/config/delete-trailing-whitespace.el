;;; delete-trailing-whitespace --- Remove trailing whitespace when saving.
;;; Commentary:
;;; Code:
(add-hooks (before-save)
	   delete-trailing-whitespace)
;;; delete-trailing-whitespace.el ends here

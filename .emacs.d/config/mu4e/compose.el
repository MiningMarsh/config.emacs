;;; compose --- Governs how mu4e composes things.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (mu4e org-mu4e)

	   (setq org-mu4e-convert-to-html t)

	   (defalias 'org-mail 'org-mu4e-compose-org-mode)
	   (defun org-export-string (data &rest rest)
	     (let ((org-html-with-latex 'imagemagick))
	       (org-export-string-as
		data 'html t))))
;;; compose.el ends here

(requires mu4e org-mu4e)

(setq org-mu4e-convert-to-html t)

(defalias 'org-mail 'org~mu4e-mime-switch-headers-or-body)
'(defalias 'org-mail 'org-mu4e-compose-org-mode)
'(defun org-export-string (data &rest rest)
   (let ((org-html-with-latex 'imagemagick))
	 (org-export-string-as
	  data 'html t)))

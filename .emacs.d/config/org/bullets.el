;;; bullets --- Enable pretty bullets in org mode.
;;; Commentary:
;;; Code:

(packages/requires (org-bullets)

	   ;; Add pretty bullets for sub-headers.
	   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

	   ;; Also pretty bullets.
	   (font-lock-add-keywords
	    'org-mode
	    '(("^ +\\([-*]\\) "
	       (0 (prog1 ()
		    (compose-region
		     (match-beginning 1)
		     (match-end 1)
		     "•")))))))

;;; bullets.el ends here

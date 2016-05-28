;;; message-filter --- Allows one to filter messages.
;;; Commentary:
;;; This package can be used to rewrite or surpress Emacs messages.
;;; Code:
(packages/define message-filter ()

  (defvar message-filters '()
    "Assoc-map of trigger to replacement functions for messages.")

  (defun message-blacklist (&rest tests)
    (dolist (test tests)
      (setq message-filters
	    (cons (cons test
			nil)
		  message-filters))))

  (defwrap message (format-string &rest args)
    (if (not format-string)
	(message nil)
      (let1 result (apply 'format (cons format-string args))
	    (dolist (filter-map message-filters)
	      (bind-head-tail (filter transform) filter-map

			      ;; Special case: strings == regex.
			      (lexical-let ((original-filter filter)
					    (original-transform transform))
				(when (stringp filter)
				  (when (and transform (stringp transform))
				    (setq transform
					  (lambda (string)
					    (replace-regexp-in-string
					     original-filter original-transform string))))
				  (setq filter
					(lambda (string)
					  (string-match
					   original-filter
					   string)))))

			      ;; Special case: if filter is not a function, make it a function that returns said value.
			      (when (not (functionp transform))
				(lexical-let ((original-transform transform))
				  (setq transform (lambda (test) original-transform))))

			      ;; Actually apply filter.
			      (when (funcall filter result)
				(setq result (funcall transform result)))

			      ;; Special case: If result is nil, quit now.
			      (when (not result)
				(message nil)
				(return))

			      ;; Special case: If result is non-nil non-string value, turn into string.
			      (when (and result (not (stringp result)))
				(setq result (format "%s" result)))))
	    (if (not result)
		(message nil)
	      (message "%s" result))))))
;;; message-filter.el ends here

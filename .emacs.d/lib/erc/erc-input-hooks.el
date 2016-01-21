;;; erc-input-hooks --- Allows you to define hooks on typed input in ERC.
;;; Commentary:
;;; Used for greentext, mostly.
;;; Code:
(require 'rc)

(defeat erc-input-hooks (erc)

  (defvar erc-input-hooks-alist '()
    "Assoc list of hook tests to hook functions.")

  (defvar erc-input-hooks-active t
    "Whether erc input hooks should fire.")

  (defun erc-add-input-hook (tester resulter)
    "Adds an erc input hook."
    (setq erc-input-hooks-alist
	  (cons
	   (cons tester resulter)
	   erc-input-hooks-alist)))

  (defwrap erc-send-input (message)
    (if (not erc-input-hooks-active)
	(erc-send-input message)
      (let1 taken nil
	    (dolist (input-hook-pair erc-input-hooks-alist)
	      (bind-head-tail (test result) input-hook-pair

			      ;; Special case: strings == regex.
			      (lexical-let ((original-test test)
					    (original-result result))
				(when (stringp test)
				  (when (and result (stringp result))
				    (setq result
					  (lambda (string)
					    (replace-regexp-in-string
					     original-test original-result string))))
				  (setq test
					(lambda (string)
					  (string-match
					   original-test
					   string)))))

			      ;; Special case: if result is not a function, make
			      ;; it a function that returns said value.
			      (when (not (functionp result))
				(lexical-let ((original-result result))
				  (setq result (lambda (test) original-result))))

			      ;; Actually apply test and filter.
			      (when (funcall test message)
				(setq message (funcall result message))
				(setq taken t))))
	    (if taken
		(dolist (input (mklist message))
		  (erc-send-input input))
	      (erc-send-input message))))))
;;; erc-input-hooks.el ends here

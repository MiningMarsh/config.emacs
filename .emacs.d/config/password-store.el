;;; pass --- Add `pass` support.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(defeat pass (password-store)

  (defun pass (entry fn)
    "Find password for ENTRY, call FN with the password as the only argument.")
  (advice-add 'password-store-copy :around
	      (lambda (fn entry)
		(with-lexical (fn entry)
			      (launch-program "ask-gpg-pin")
			      (set-process-sentinel
			       (get-process "ask-gpg-pin")
			       (lambda (process state)
				 (when (string= "finished\n" state)
				   (funcall fn entry))))))))
;;; pass.el ends here

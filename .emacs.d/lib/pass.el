;;; pass --- Add `pass` support.
;;; Commentary:
;;; Code:
(require 'rc)

(setq lexical-binding t)

(defeat pass (password-store)

  (defvar pass/pinentry-program "ask-gpg-pin"
    "The program to run to get user gpg pin.")

  (defun pass/-run-after-pinentry (fn)
    "Run FN after pinentry has been run."
    (launch-program pass/pinentry-program)
    (set-process-sentinel
     (get-process pass/pinentry-program)
     (lambda (process state)
       (if (string= "finished\n" state)
	   (funcall fn)
	 (pass/-run-after-pinentry fn)))))

  (cl-defmacro pass/-after-pinentry (&body body)
    "Run BODY after pinentry has been run and user pin is cached."
    `(pass/-run-after-pinentry (lambda () ,@body)))

  (defun pass/get-password (entry fn)
    "Find password for ENTRY, call FN with the password as the only argument."
    (pass/-after-pinentry
     (funcall fn (password-store-get entry))))

  (cl-defmacro pass/with-password ((password entry) &body body)
    "Bind PASSWORD to the password for ENTRY, then run BODY with PASSWORD bound.
This function DOES NOT return the result of BODY, due to some issues involving async."
    `(pass/get-password ,entry (lambda (,password) ,@body))))
;;; pass.el ends here

;;; mu4e-accounts --- Adds sane account support to mu4e.
;;; Commentary:
;;; Modified version of Bryan's shit.
;;; Code:
(require 'rc)

(defeat mu4e-accounts (mu4e mu4e-fixes)

  (cl-defstruct mu4e-account
    name
    drafts-folder
    mail-address
    sent-folder
    smtp-port
    smtp-server
    smtp-stream-type
    trash-folder)

  (defvar mu4e-account-list
    (list (make-mu4e-account
	   :name "Online"
	   :drafts-folder "/miningmarsh/[Gmail].Drafts"
	   :mail-address "miningmarsh@gmail.com"
	   :sent-folder "/miningmarsh/[Gmail].Sent Mail"
	   :smtp-port 465
	   :smtp-server "smtp.gmail.com"
	   :smtp-stream-type 'ssl
	   :trash-folder "/miningmarsh/[Gmail].Trash")
	  (make-mu4e-account
	   :name "School"
	   :drafts-folder "/jsongy3/[Gmail].Drafts"
	   :sent-folder "/jsongy3/[Gmail].Sent Mail"
	   :mail-address "jsongy3@gmail.com"
	   :smtp-port 465
	   :smtp-server "smtp.gmail.com"
	   :smtp-stream-type 'ssl
	   :trash-folder "/jsongy3/[Gmail].Trash")
	  (make-mu4e-account
	   :name "Personal"
	   :drafts-folder "/joshua.songy/[Gmail].Drafts"
	   :mail-address "joshua.songy@gmail.com"
	   :sent-folder "/joshua.songy/[Gmail].Sent Mail"
	   :smtp-port 465
	   :smtp-server "smtp.gmail.com"
	   :smtp-stream-type 'ssl
	   :trash-folder "/joshua.songy/[Gmail].Trash"))
    "List of mu4e accounts.")

  (defun apply-mu4e-account (mu4e-account)
    (setq mu4e-sent-folder (mu4e-account-sent-folder mu4e-account)
	  mu4e-drafts-folder (mu4e-account-drafts-folder mu4e-account)
	  mu4e-trash-folder (mu4e-account-trash-folder mu4e-account)
	  user-mail-address (mu4e-account-mail-address mu4e-account)
	  smtpmail-default-smtp-server (mu4e-account-smtp-server mu4e-account)
	  smtpmail-smtp-server (mu4e-account-smtp-server mu4e-account)
	  smtpmail-stream-type (mu4e-account-smtp-stream-type mu4e-account)
	  smtpmail-smtp-service (mu4e-account-smtp-port mu4e-account))
    nil)

  (add-hooks (after-init-hook)
	     (when mu4e-account-list
	       (-> mu4e-account-list
		   car
		   apply-mu4e-account)))

  (defun switch-mu4e-account ()
    (interactive)
    (let1 account-name (completing-read
			"Switch to account: "
			(mapcar 'mu4e-account-name mu4e-account-list)
			nil t nil nil (mu4e-account-name (car mu4e-account-list)))
	  (if account-name
	      (let1 filtered-list (cl-remove-if-not
				   (lambda (account)
				     (string= account-name
					      (mu4e-account-name account)))
				   mu4e-account-list)
		    (if filtered-list
			(-> filtered-list
			    car
			    apply-mu4e-account)
		      (error "No email account found")))
	    (error "No email account found"))))

  (add-hooks (mu4e-compose-pre-hook)
	     switch-mu4e-account))
;;; mu4e-accounts.el ends here

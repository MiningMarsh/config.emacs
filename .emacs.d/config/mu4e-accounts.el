;;; mu4e-accounts --- Adds sane account support to mu4e.
;;; Commentary:
;;; Modified version of Bryan's shit.
;;; Code:
(require 'rc)

(defeat mu4e-accounts (mu4e mu4e-fixes)

  (cl-defstruct mu4e-account
    drafts-folder
    mail-address
    sent-folder
    smtp-port
    smtp-server
    stream-type
    trash-folder)

  (defvar mu4e-accounts-alist
    (list
     (make-mu4e-account
      :drafts-folder "/miningmarsh/[Gmail].Drafts"
      :mail-address "miningmarsh@gmail.com"
      :sent-folder "/miningmarsh/[Gmail].Sent Mail"
      :smtp-port 465
      :smtp-server "smtp.gmail.com"
      :stream-type 'ssl
      :trash-folder "/miningmarsh/[Gmail].Drafts"))
    "List of mu4e accounts.")

  (defvar my-mu4e-account-alist
    (assoc-map
     "joshua.songy@gatech.edu"
     (var-map mu4e-sent-folder             "/jsongy3/[Gmail].Sent Mail"
	      mu4e-drafts-folder           "/jsongy3/[Gmail].Drafts"
	      mu4e-trash-folder            "/jsongy3/[Gmail].Trash"
	      user-mail-address            "jsongy3@gmail.com"
	      smtpmail-default-smtp-server "smtp.gmail.com"
	      smtpmail-smtp-server         "smtp.gmail.com"
	      smtpmail-stream-type         'ssl
	      smtpmail-smtp-service        465)
     "miningmarsh@gmail.com"
     (var-map mu4e-sent-folder             "/miningmarsh/[Gmail].Sent Mail"
	      mu4e-drafts-folder           "/miningmarsh/[Gmail].Drafts"
	      mu4e-trash-folder            "/miningmarsh/[Gmail].Trash"
	      user-mail-address            "miningmarsh@gmail.com"
	      smtpmail-default-smtp-server "smtp.gmail.com"
	      smtpmail-smtp-server         "smtp.gmail.com"
	      smtpmail-stream-type         'ssl
	      smtpmail-smtp-service        465))
    "List of mu4e accounts.")

  ;; Set default account.
  (-> "joshua.songy@gatech.edu" (assocr my-mu4e-account-alist) apply-var-map)

  (add-hooks (mu4e-compose-pre-hook)
	     (let*
		 ((account
		   (if mu4e-compose-parent-message
		       (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
			 (string-match "/\\(.*?\\)/" maildir)
			 (match-string 1 maildir))
		     (completing-read
		      (format "Compose with account: (%s) "
			      (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
		      (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
		      nil t nil nil (caar my-mu4e-account-alist))))
		  (account-vars (cdr (assoc account my-mu4e-account-alist))))
	       (if account-vars
		   (apply-var-map account-vars)
		 (error "No email account found")))))
;;; mu4e-accounts.el ends here

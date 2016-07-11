;;; docker --- Enable editing remote files in a docker container.
;;; Commentary:
;;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
;;; Code:

;; All code stolen from: https://www.emacswiki.org/emacs/TrampAndDocker

(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "Docker image container name completion for TRAMP."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
	     (dockernames (cl-remove-if-not
			   #'(lambda (dockerline) (string-match ":$" dockerline))
			   (split-string dockernames-raw "\n"))))
	(setq ad-return-value dockernames))
    ad-do-it))
;;; docker.el ends here

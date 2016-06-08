;;; path --- Add OS X specific path specification.
;;; Commentary:
;;; Code:
(packages/requires (os)
		   (os/when-mac
		    (mapc (lambda (path)

			    ;; Only add the path when it exists.
			    (when (file-exists-p (format "%s/%s"
							 (getenv "HOME")
							 path))

			      ;; Add to the path.
			      (setenv "PATH" (format "%s/%s:%s"
						     (getenv "HOME")
						     path (getenv
							   "PATH")))))
			  ;; List of paths to ensure.
			  '(".bin"
			    ".homebrew/bin"))))
;;; path.el ends here

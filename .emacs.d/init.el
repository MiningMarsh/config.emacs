;;; init --- Bootstraps emacs instance.
;;; Commentary:
;;; Loads all files in ~/.emacs.d/config recursively, and also adds
;;; ~/.emacs.d/lib as a library path for require.  The files are loaded in sorted
;;; order by name.  Also loads ~/.emacs.d/rc.el before everything else.
;;; Code:
(require 'cl-lib)

;; Wrap everything in labels so that this file does not pollute the global
;; namespace at all.
(cl-labels
    ((map-dir (fn dir)
	      "Maps a function over a directory structure."
	      (funcall fn dir)
	      (dolist (path
		       (cl-remove-if (lambda (path) (string-match ".*[.][.]?$" path))
				     (directory-files dir t)))
		(if (file-directory-p path)
		    (map-dir fn path)
		  (funcall fn path))))

     (needs-recompile (path compiled)
		      "Checks if a file needs to be re-compiled."
		      (file-newer-than-file-p path compiled))

     (compile-and-load (path compiled &optional load)
		       "Compiles and loads a file."
		       (if (needs-recompile path compiled)
			   (progn
				(let ((local (concat path "c")))
				  (when (file-exists-p local)
					(delete-file local))
				 (byte-compile-file path load)
					(when (not (string= compiled local))
					  (when (file-exists-p compiled)
						(delete-file compiled))
					  (rename-file local compiled))))
			 (when load (load-file compiled))))

     (elisp-filep (path)
		    "Returns whether the path is elisp."
		    (and path
			 (not (file-directory-p path))
			 (string-match ".*el$" path)))

     (compile-file (path &optional load)
		   "Recompiles a file if needed."
		   (compile-and-load path (concat path "c") load))

     (compile-file-cached (path &optional load)
			  "Compiles a file and puts it in the storage cache."
			  (compile-and-load
			   path
			   (format "~/.emacs.d/compiled/%sc"
				   (replace-regexp-in-string
				    "/" "!"
				    path))
			   load)))

  (cl-macrolet ((with-ignored-errors (&rest body)
		  "Ignore errors in the wrapped BODY."
		  `(unwind-protect
		       (let (retval)
			 (condition-case ex
			     (setq retval (progn ,@body))
			   ('error
			    (message "Failed to load file.")
			    (setq retval nil)))
			 retval))))


    ;; Only recompile the init file if needed.
    (compile-file "~/.emacs.d/init.el")

    ;; Make the compiled cached directory if needed.
    (when (not (file-directory-p "~/.emacs.d/compiled"))
      (make-directory "~/.emacs.d/compiled"))

    ;; Distro specific emacs library code.
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/")

    ;; Setup the library path.
    (map-dir
     (lambda (path)
       (when (file-directory-p path)
	 (add-to-list 'load-path path)))
     "~/.emacs.d/lib/")

    ;; Setup the themepath.
    (map-dir
     (lambda (path)
       (when (file-directory-p path)
	 (add-to-list 'custom-theme-load-path path)))
     "~/.emacs.d/theme/")

    ;; Byte compile everything.
    (mapc (lambda (path)
	    (map-dir
	     (lambda (path)
	       (when (elisp-filep path)
		 (with-ignored-errors (compile-file path))))
	     (format "~/.emacs.d/%s/" path)))
	  '("lib"
	    "theme"))

    ;; Load files.
    (map-dir
     (lambda (path)
       (when (elisp-filep path)
	 (with-ignored-errors (compile-file-cached path t))))
     "~/.emacs.d/config/")

    ;; Clear the message buffer.
    (message "")

    ;; Signal that init has finished.
    (provide 'init)))
;;; init.el ends here

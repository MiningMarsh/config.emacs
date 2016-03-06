;;; init --- Bootstraps emacs instance.
;;; Commentary:
;;; Loads all files in ~/.emacs.d/config recursively, and also adds
;;; ~/.emacs.d/lib as a library path for require.  The files are loaded in sorted
;;; order by name.  Also loads ~/.emacs.d/rc.el before everything else.
;;; Code:
(require 'cl-lib)

;; init.el hook
(defvar after-config-hook nil
  "List of hooks to run after config has been read.")

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

     (create-message (&rest strs)
		     (concat
		      (cl-reduce
		       (lambda (l r)
			 (concat l "\n" r))
		       strs)
		      "\n"))

     (float-current-time ()
			 "Return float time, ignores upper bits."
			 (let ((current (current-time)))
			   (+ (second current) (/ (third current) 10000000.0))))

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
					    retval)))
		(elapsed-time (&rest body)
			      "Execute body, returning time elapsed."
			      (let ((start-time (cl-gensym)))
				`(let ((,start-time (float-current-time)))
				   ,@body
				   (- (float-current-time) ,start-time)))))


    (let ((load-time 0)
	  (walk-time 0)
	  (lib-time 0)
	  (config-time 0)
	  (package-time 0)
	  (userp 0)
	  (systemp 0)
	  (combinedp 0))
      (setq load-time
	    (elapsed-time
	     ;; Only recompile the init file if needed.
	     (compile-file "~/.emacs.d/init.el")

	     ;; Make the compiled cached directory if needed.
	     (when (not (file-directory-p "~/.emacs.d/compiled"))
	       (make-directory "~/.emacs.d/compiled"))

	     ;; Distro specific emacs library code.
	     (add-to-list 'load-path "/usr/share/emacs/site-lisp/")

	     (setq
	      walk-time
	      (elapsed-time
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
		"~/.emacs.d/theme/")))

	     (setq
	      lib-time
	      (elapsed-time
	       ;; Byte compile everything.
	       (mapc (lambda (path)
		       (map-dir
			(lambda (path)
			  (when (elisp-filep path)
			    (with-ignored-errors (compile-file path))))
			(format "~/.emacs.d/%s/" path)))
		     '("lib" "theme"))))

	     (setq
	      config-time
	      (elapsed-time
	       ;; Load files.
	       (map-dir
		(lambda (path)
		  (when (elisp-filep path)
		    (with-ignored-errors (compile-file-cached path t))))
		"~/.emacs.d/config/")))

	     ;; Run post config hooks.
	     (dolist (hook after-config-hook)
	       (funcall hook))

	     ;; Package query logic.
	     (setq
	      package-time
	      (elapsed-time
	       (setq userp (length package-alist)
		     systemp (length package--builtins)
		     combinedp (length
				(remove-duplicates
				 (mapcar 'car
					 (append package-alist
						 package--builtins)))))))))

      ;; Clear the message buffer.
      (message "")

      ;; Make sure we only have one window.
      (delete-other-windows)

      ;; Switch to scratch buffer.
      (switch-to-buffer "*scratch*")

      ;; Print stats to user
      (setq initial-scratch-message
	    (format (create-message ";;; This buffer is for quick Emacs Lisp code."
				    ";;;"
				    ";;; Startup took %f seconds."
				    ";;; ➡ Directory walking took %f seconds."
				    ";;; ➡ Library compilation took %f seconds."
				    ";;; ➡ Configuration compilation and loading took %f seconds."
				    ";;; ➡ Package query logic took %f seconds."
				    ";;;"
				    ";;; There are %d packages installed."
				    ";;; ➡ There are %d builtin packages installed."
				    ";;; ➡ There are %d user-requested packages installed."
				    ";;; ➡ %d packages were installed on startup."
				    ";;; ➡ %d packages were removed on startup."
				    )
		    load-time
		    walk-time
		    lib-time
		    config-time
		    package-time
		    combinedp
		    systemp
		    userp
		    package-installed-on-startup
		    package-removed-on-startup))

      ;; Signal that init has finished.
      (provide 'init))))
;;; init.el ends here

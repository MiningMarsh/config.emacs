;;; bootstrap --- Bootstraps emacs instance.
;;; Commentary:
;;; Loads all files in ~/.emacs.d/config recursively, and also adds
;;; ~/.emacs.d/lib as a library path for require.  The files are loaded in sorted
;;; order by name.  Also loads ~/.emacs.d/rc.el before everything else.
;;; Code:
(require 'cl-lib)

(defvar bootstrap/after-config-hook nil
  "List of hooks to run after config has been read.")

(defun bootstrap/-directory-files-ignoring-implicit (&rest args)
  "ARGS Passthrough for `directory-files` that will remove '.' and '..' from the results."
  (cl-remove-if (lambda (path) (string-match ".*[.][.]?$" path))
		(apply (lambda (path) (directory-files path t)) args)))

(defun bootstrap/-map-dir (fn dir)
  "Maps the function FN over directory DIR."
  (funcall fn dir)
  (dolist (path (bootstrap/-directory-files-ignoring-implicit dir))
    (if (file-directory-p path)
	(bootstrap/-map-dir fn path)
      (funcall fn path))))

(defun bootstrap/-needs-recompile? (path compiled)
  "Check if PATH is latest, or if COMPILED must be regenerated."
  (or (not (file-exists-p compiled))
      (file-newer-than-file-p path compiled)))

(defun bootstrap/-compile-and-load (path compiled &optional load)
  "Compile PATH to COMPILED, and optionally load it if LOAD is set."
  (if (not (bootstrap/-needs-recompile? path compiled))
      ;; Otherwise, just load the compiled file if requested.
      (when load (load-file compiled))

    ;; Generate the local compiled path.
    (let ((local (concat path "c")))

      ;; When the compiled file already exists, remove it to prevent a file
      ;; conflict.
      (when (file-exists-p local)
	(delete-file local))

      ;; Compile the file and load if needed.
      (byte-compile-file path load)

      ;; If the requested compile path is not the same as the local compiled
      ;; path, move the newly generated file to the correct place.
      (when (not (string= compiled local))

	;; Delete the requested file if we are replacing it.
	(when (file-exists-p compiled)
	  (delete-file compiled))

	;; Move the newly compiled file.
	(rename-file local compiled)))))

(defun bootstrap/-elisp-file? (path)
  "Check if PATH is an elisp file."
  (and path
       (not (file-directory-p path))
       (string-match ".*el$" path)))

(defun bootstrap/-compile-file (path &optional load)
  "Compile PATH and load it if LOAD is set."
  (bootstrap/-compile-and-load path (concat path "c") load))

(defun bootstrap/-concat-message (&rest strs)
  "Helper function used to format the scratch buffer message.  Concats STRS."
  (concat (cl-reduce (lambda (l r) (concat l "\n" r)) strs)
	  "\n"))

(defun bootstrap/-current-time-float ()
  "Return float time, ignoring upper bits."
  (let ((current (current-time)))
    (+ (cl-second current) (/ (cl-third current) 10000000.0))))

(defun bootstrap/-cached-path (path)
  "Return the cache version of PATH."
  (format "~/.emacs.d/compiled/%sc"
	  (replace-regexp-in-string "/" "!" path)))

(defun bootstrap/-compile-and-cache-file (path &optional load)
  "Compile PATH and put it in the storage cache, then load it if LOAD is set."
  (bootstrap/-compile-and-load
   path
   (bootstrap/-cached-path path)
   load))

(defmacro bootstrap/-with-ignored-errors (&rest body)
  "Ignore any errors raised in the execution of BODY."
  `(unwind-protect
       (let (retval)
	 (condition-case ex
	     (setq retval (progn ,@body))
	   ('error
	    (message "Failed to load file.")
	    (setq retval nil)))
	 retval)))

(defmacro bootstrap/-elapsed-time (&rest body)
  "Execute BODY, returning time elapsed."
  (let ((start-time (cl-gensym)))
    `(let ((,start-time (bootstrap/-current-time-float)))
       ,@body
       (- (bootstrap/-current-time-float) ,start-time))))

(defmacro bootstrap/-record-elapsed-time (var &rest body)
  "Set VAR to the time it took to execute BODY."
  `(setq ,var (bootstrap/-elapsed-time ,@body)))

(defvar bootstrap/startup-time 0
  "The amount of time the bootstrapper spent loading.")

(bootstrap/-record-elapsed-time
 bootstrap/startup-time

 ;; Only recompile the init file if needed.
 (bootstrap/-compile-file "~/.emacs.d/init.el")

 ;; Make the compiled cached directory if needed.
 (when (not (file-directory-p "~/.emacs.d/compiled"))
   (make-directory "~/.emacs.d/compiled"))

 ;; Distro specific emacs library code.
 (add-to-list 'load-path "/usr/share/emacs/site-lisp/")

 ;; Setup the library path.
 (bootstrap/-map-dir
  (lambda (path)
    (when (file-directory-p path)
      (add-to-list 'load-path path)))
  "~/.emacs.d/lib/")

 ;; Setup the themepath.
 (bootstrap/-map-dir
  (lambda (path)
    (when (file-directory-p path)
      (add-to-list 'custom-theme-load-path path)))
  "~/.emacs.d/theme/")

 ;; Load the package manager and standard library if we haven't already.
 (require 'rc)
 (require 'packages)

 ;; Byte compile everything.
 (mapc (lambda (path)
	 (bootstrap/-map-dir
	  (lambda (path)
	    (when (bootstrap/-elisp-file? path)
	      (bootstrap/-with-ignored-errors (bootstrap/-compile-file path))))
	  (format "~/.emacs.d/%s/" path)))
       '("lib" "theme"))

 ;; Load files.
 (bootstrap/-map-dir
  (lambda (path)
    (when (bootstrap/-elisp-file? path)
      (bootstrap/-with-ignored-errors (bootstrap/-compile-and-cache-file path t))))
  "~/.emacs.d/config/")

 ;; Run post config hooks.
 (dolist (hook bootstrap/after-config-hook)
   (funcall hook)))

;; Clear the message buffer.
(message "")

;; Make sure we only have one window.
(delete-other-windows)

;; Switch to scratch buffer.
(switch-to-buffer "*scratch*")

;; Print stats to user
(let ((userp (length package-alist))
      (systemp (length package--builtins))
      (combinedp
       (length
	(remove-duplicates
	 (mapcar 'car (append package-alist package--builtins))))))
  (setq initial-scratch-message
	(format (bootstrap/-concat-message ";;; This buffer is for quick Emacs Lisp code."
					   ";;;"
					   ";;; Startup took %f seconds."
					   ";;;"
					   ";;; There are %d packages installed."
					   ";;; ➡ There are %d builtin packages installed."
					   ";;; ➡ There are %d user-requested packages installed."
					   ";;; ➡ %d packages were added on startup."
					   ";;; ➡ %d packages were upgraded on startup."
					   ";;; ➡ %d packages were removed on startup."
					   )
		bootstrap/startup-time
		combinedp
		systemp
		userp
		packages/installed
		packages/upgraded
		packages/removed)))

;; Signal that init has finished.
(provide 'bootstrap)
;;; bootstrap.el ends here

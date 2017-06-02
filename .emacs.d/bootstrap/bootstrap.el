;;; bootstrap --- Bootstraps emacs instance.
;;; Commentary:
;;; Loads all files in ~/.emacs.d/config recursively, and also adds
;;; ~/.emacs.d/lib as a library path for require.  The files are loaded in sorted
;;; order by name.  Also loads ~/.emacs.d/rc.el before everything else.
;;; Code:
;; We use a bunch of the CL compatibility functions to make this file a bit
;; shorter.
(require 'cl-lib)

;; We provide bootstrap to let programs know that the bootstrapper has run.
;; This is not terribly useful, but can be used to ping whether you are
;; running with a bootstrapper.
(provide 'bootstrap)

;; This is provided primarily so that the custom package manager can clean
;; orphaned packages after startup.
(defvar bootstrap/after-config-hook nil
"List of hooks to run after config has been read.")

;; Needed so that map-dir doesn't try mapping over '.' or '..' thereby
;; recursing infinitely.
(defun bootstrap/-directory-files-ignoring-implicit (&rest args)
"ARGS Passthrough for `directory-files` that will remove '.' and '..' from
the results."
  (cl-remove-if (lambda (path) (string-match ".*[.][.]?$" path))
    (apply (lambda (path) (directory-files path t)) args)))

;; Used so that we can map our load function over the entire config directory,
;; etc.
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

(defmacro bootstrap/-with-finally (fn &rest body)
"Ignore errors in the wrapped FN, executing BODY as finally."
  `(unwind-protect
     (let (retval)
       (condition-case ex
         (setq retval (progn ,fn))
       ('error
         (setq retval nil)))
       retval)
     ,@body))

(defun bootstrap/-prepend-require (path)
"Prepend requires to PATH in a temp file, return the temp file path."
  (let ((temp-file (make-temp-file "emacs-config-file-")))
    (delete-file temp-file)
    (setq temp-file (concat temp-file ".el"))
    (with-temp-file temp-file
      (insert-file-contents path)
      (goto-char (point-min))
      (insert "(require 'all)\n")
      temp-file)))

(defun bootstrap/-compile-and-load (path compiled &optional load ignore-includes)
"Compile PATH to COMPILED, and optionally load it if LOAD is set.
If IGNORE-INCLUDES is set, don't prepend (require 'all) to the file."
  (when load
    )
  (if (not (bootstrap/-needs-recompile? path compiled))
      ;; Otherwise, just load the compiled file if requested.
      (when load
	(message "Loading %s..." path)
	(load-file compiled))

    ;; Prepend the config if needed.
    (unless ignore-includes
      (setq path (bootstrap/-prepend-require path)))

    ;; Generate the local compiled path.
    (let ((local (concat path "c")))

      ;; When the compiled file already exists, remove it to prevent a file
      ;; conflict.
      (when (file-exists-p local)
        (delete-file local))

      ;; Ensure that the temp files are always deleted.
      (bootstrap/-with-finally
        (progn

	  ;; Signal that we are compiling or loading.
	  (if load
	      (message "Loading %s..." path)
	    (message "Compiling %s..." path))

          ;; Compile the file and load if needed.
          (byte-compile-file path load)

          ;; If the requested compile path is not the same as the local compiled
          ;; path, move the newly generated file to the correct place.
          (when (not (string= compiled local))

          ;; Delete the requested file if we are replacing it.
          (when (file-exists-p compiled)
            (delete-file compiled))

          ;; Move the newly compiled file.
          (rename-file local compiled)))

      ;; Delete the temp file if needed.
      (unless ignore-includes
        (delete-file path)

      (when (file-exists-p local)
        (delete-file local)))))))

(defun bootstrap/-elisp-file? (path)
"Check if PATH is an elisp file."
  (and path
       (not (file-directory-p path))
       (string-match ".*el$" path)))

(defun bootstrap/-compile-file (path &optional load bootstrap)
"Compile PATH and load it if LOAD is set.  Add libraries if not BOOTSTRAP."
  (bootstrap/-compile-and-load path (concat path "c") load bootstrap))

(defmacro bootstrap/-concat-message (&rest strs)
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

(bootstrap/-record-elapsed-time bootstrap/startup-time

  ;; Only recompile the init file if needed.
  (bootstrap/-compile-file "~/.emacs.d/init.el" nil t)

  ;; Make needed directories.
  (mapc (lambda (path)
          (setq path (format "~/.emacs.d/%s" path))
          (when (not (file-directory-p path))
            (make-directory path)))
        '("compiled"
          "lib"
	  "theme"
          "persistent"
          "config"))

 ;; Distro specific emacs library code.
 (add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; Make sure we only have one window.
(delete-other-windows)

;; Switch to messages buffer.
(switch-to-buffer "*Messages*")

 ;; Setup the library path.
 (mapc (lambda (path)
	 (bootstrap/-map-dir
	  (lambda (path)
	    (when (file-directory-p path)
	      (add-to-list 'load-path path)))
	  (format "~/.emacs.d/%s/" path)))
       '("lib" "bootstrap"))

 ;; Setup the themepath.
 (bootstrap/-map-dir
  (lambda (path)
    (when (file-directory-p path)
      (add-to-list 'custom-theme-load-path path)))
  "~/.emacs.d/theme/")

 ;; Byte compile bootstrap libraries.
 (bootstrap/-map-dir
  (lambda (path)
    (when (bootstrap/-elisp-file? path)
      (bootstrap/-with-ignored-errors (bootstrap/-compile-file path nil t))))
  "~/.emacs.d/bootstrap/")

 ;; Byte compile lib libraries.
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
      (bootstrap/-with-ignored-errors
       (bootstrap/-compile-and-cache-file path t))))
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
	(format (bootstrap/-concat-message
		 ";;; This buffer is for quick Emacs Lisp code."
		 ";;;"
		 ";;; Startup took %f seconds."
		 ";;;"
		 ";;; There are %d packages installed."
		 ";;; ➡ There are %d builtin packages installed."
		 ";;; ➡ There are %d user-requested packages installed."
		 ";;; ➡ %d packages were added on startup."
		 ";;; ➡ %d packages were upgraded on startup."
		 ";;; ➡ %d packages were removed on startup.")
		bootstrap/startup-time
		combinedp
		systemp
		userp
		packages/installed
		packages/upgraded
		packages/removed)))
;;; bootstrap.el ends here

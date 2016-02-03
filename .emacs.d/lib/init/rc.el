;;; rc --- Custom standard library.
;;; Commentary:
;;; Standard library that can always be assumed to be loaded when writing
;;; config files.
;;; Code:
(require 'cl-lib)

;; Turn on lexical binding by default.
(setq lexical-binding t)

(defun clear-directory (path)
  "Clears the contents of the given directory."
  (delete-directory path t)
  (make-directory path))

(defun move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defmacro requires (&rest libs)
  "Lets you require multiple things without quoting them."
  `(mapc (symbol-function 'require)
	 (list ,@(mapcar (lambda (lib) `(quote ,lib)) libs))))

(defmacro provides (&rest libs)
  "Lets you provide multiple things without quoting them."
  `(mapc (symbol-function 'provide)
	 (list ,@(mapcar (lambda (lib) `(quote ,lib)) libs))))

(defun mklist (thing)
  "Ensures something is a list."
  (if (listp thing)
      thing
    (list thing)))

(defmacro requiring (libs &rest body)
  "Lets you require multiple things without quoting them."
  `(when (and
	  ,@(mapcar
	     (lambda (lib)
	       `(if (require ',lib nil 'noerror)
		    t
		  (progn
		    (message ,(concat "Failed to load library: " (symbol-name lib)))
		    nil)))
	     libs))
     ,@body))

(defmacro defeat (feature-name deps &rest body)
  `(requiring ,deps
	      ,@body
	      (provides ,feature-name)))

(defun compiled-file-path (path)
  "Returns the compiled file path for file."
  (format "%sc" path))

(cl-defmacro save-buffer-info (&rest body)
  "Saves information about the current buffer, runs body, and restores info."
  `(save-excursion
	 (save-restriction
	   ,@body)))

(defun buffer-starts-with (regex)
  "Returns whether the current buffer starts with a regex."
  (save-buffer-info
	(widen)
	(goto-char (point-min))
	(save-match-data
	  (looking-at regex))))

(defun buffer-is-script ()
  "Returns whether the current buffer is a script."
  (buffer-starts-with "^#!"))

(cl-defmacro let1 (var value &rest body)
  "Makes reading a single let easier."
  `(let ((,var ,value))
     ,@body))

(defun mappend (fn &rest args)
  "Like mapcan, but uses append instead of nconc."
  (apply #'mapcan (lambda (thing) (cl-copy-list (funcall fn thing))) args))

(defun to-list (thing)
  "Converts a thing into a list."
  (if (listp thing)
      (cl-copy-list thing)
    (list thing)))

(defun flatten-once (tree)
  "Flattens a list by a single level."
  (mappend #'to-list tree))

(defun flatten (tree)
  "Fully flattens a list."
  (mappend
   (lambda (thing)
     (if (listp thing)
	 (flatten thing)
       (list thing)))
   tree))

(defun emptyp (sequence)
  "Checks if a sequence is empty."
  (zerop (length sequence)))

(defun partition (length list)
  "Partitions a list."
  (loop
   while list
   collect (cl-subseq list 0 length)
   do (setf list (nthcdr length list))))

(defmacro assoc-map (&rest map-list)
  "Makes writing assoc lists nicer."
  `(list
    ,@(mapcar
       (lambda (e) `(cons ,(car e) ,(cadr e)))
       (partition 2 map-list))))

(defmacro var-map (&rest map-list)
  "Makes writing variable mappings nicer."
  `(list
    ,@(mapcar
       (lambda (e) `(cons (quote ,(car e)) ,(cadr e)))
       (partition 2 map-list))))

(defun apply-var-map (var-map)
  "Sets the var map given."
  (mapc (lambda (mapping) (set (car mapping) (cdr mapping)))
	var-map))

(defmacro with-gensyms (syms &rest body)
  "Evalutes body with the given symbols bound to gensyms."
  `(let
       ,(mapcar
	 (lambda (sym) `(,sym (cl-gensym)))
	 syms)
     ,@body))

(defmacro with-gensym (sym &rest body)
  "Easier to read than with-gensyms for one var."
  `(with-gensyms (,sym)
		 ,@body))

(cl-defmacro bind-head-tail ((head tail) list-expr &rest body)
  "Anaphoric macro that binds the car and cdr of a list to specified atoms.
   This is used often."
  ;; Used so we only evaluate the list once.
  (with-gensym result
	       `(let* ((,result ,list-expr)
		       (,head (car ,result))
		       (,tail (cdr ,result)))
		  ,@body)))

(cl-defmacro -> (&rest exprs)
  "Threading macro ported from clojure."
  (bind-head-tail (first rest) exprs
		  (if (not rest)
		      first
		    (bind-head-tail (second rest) rest
				    (if (listp second)
					`(-> (,(car second) ,first ,@(cdr second)) ,@rest)
				      `(-> (,second ,first) ,@rest))))))

(cl-defmacro ->> (&rest exprs)
  "Threading macro ported from clojure."
  (bind-head-tail (first rest) exprs
		  (if (not rest)
		      first
		    (bind-head-tail (second rest) rest
				    (if (listp second)
					`(->> (,@second ,first) ,@rest)
				      `(->> (,second ,first) ,@rest))))))

(defmacro locals (&rest exprs)
  "Makes using an interior function definition easier than with labels
(or at least easier to read)."
  (cl-labels
      ((generate-local-expr (name args body)
			    "Generate a local expression, without the body appended."
			    `(cl-labels
				 ((,name ,args
					 ,@body))))

       (is-local (expr)
		 "Test if this is a deflocal expression."
		 (when (listp expr)
		   (when (eq (car expr) 'deflocal)
		     t))))

    ;; The accumulator and a link to the start of the entire acc chain.
    (let* ((acc (list 'progn))
	   (cur acc))

      ;; Repeat until we have processed the entire expr list.
      (while exprs
	(bind-head-tail (head tail) exprs

			;; Append the next expr to the acc.
			(if (not (is-local head))
			    ;; If it is not a local, just append it to the last set of
			    ;; expressions to be executed.
			    (nconc cur (list head))
			  ;; Otherwise, generate a new nested labels, and append that to the
			  ;; executed expressions, then advance our pointer into the acc to
			  ;; the new nested labels expression.
			  (let ((new (generate-local-expr
				      (cadr head)
				      (caddr head)
				      (cdddr head))))
			    (nconc cur (list new))
			    (setq cur new)))
			(setq exprs (cdr exprs))))

      ;; Return the resultant accumulator.
      acc)))

(defmacro add-hooks (hook-names &rest hooks)
  "Add HOOKS to the specified HOOK-NAMES."
  (locals

   (deflocal generate-add-hook (hook-name hook)
     "Generates an add-hook expression."
     `(add-hook
       (quote ,hook-name)
       ,(if (listp hook)
	    `(lambda () ,hook)
	  `(lambda () (,hook)))
       t))

   ;; Generates an add-hook for every expression possible.
   `(progn
      ,@(mapcar
	 (lambda (name)
	   `(progn ,@(mapcar
		      (lambda (hook)
			(generate-add-hook name hook))
		      hooks)))
	 hook-names))))

(defun assocr (value map)
  "Like (assoc VALUE MAP), but just return cdr."
  (-> value
      (assoc map)
      cdr))

(defun extract-line-number (line-str)
  "Return line number represented by LINE-STR."
  (-> line-str
	  (cl-subseq 5)
	  string-to-number))

(defun current-line-number ()
  "Return the current line number."
  (-> (what-line)
	  extract-line-number))

(defun lines-in-buffer ()
  "Return the number of lines in the current buffer."
  (save-buffer-info
   (end-of-buffer)
   (current-line-number)))

(defmacro defwrap (name args-list &rest body)
  "Define a wrapper function for function NAME, taking arguments ARGS-LIST, with code body of BODY.  The original function is implicitly bound to the name NAME in the code body."
  (with-gensym fn
	       `(advice-add (quote ,name) :around
			    (lambda ,(cons fn args-list)
			      (cl-labels ((,name (&rest args) (apply ,fn args)))
				,@body)))))

(defun regexp (regex)
  "Generate a REGEX matching predicate."
  (let ((regex regex))
    (lambda (string)
      (string-match regex string))))

(defun current-emacs-client ()
  "Return the current Emacs server client."
  (frame-parameter nil 'client))

(defun kill-current-emacs-client ()
  "Kill the current Emacs server client."
  (-> (current-emacs-client)
      server-delete-client))

(defmacro with-ignored-errors (&rest body)
  "Ignore errors in the wrapped BODY."
  `(unwind-protect
       (let (retval)
	 (condition-case ex
	     (setq retval (progn ,@body))
	   ('error
	    (setq retval nil)))
	 retval)))

(defmacro with-finally (fn &rest body)
  "Ignore errors in the wrapped FN, executing BODY as finally."
  `(unwind-protect
       (let (retval)
	 (condition-case ex
	     (setq retval (progn ,fn))
	   ('error
	    (setq retval nil)))
	 retval)
     ,@body))

(defmacro on-error (fn &rest body)
  "Execute FN, and if it encounters an error, run BODY."
  `(let1 no-error nil
	 (with-ignored-errors
	  (progn ,fn
		 (setq no-error t)))
	 (when (not no-error)
	     ,@body)))

(defmacro with-finally-returned (fn &rest body)
  "Ignore errors in the wrapped FN, executing and returning BODY as finally."
  `(progn (with-ignored-errors ,fn)
	  ,@body))

(defun play-youtube (url)
  "Plays a youtube video URL."
  (start-process url url "mpv" url))

(defun remove-from-list (list element &optional compare-fn)
  "Remove from LIST ELEMENT, using COMPARE-FN."
  (set list (cl-delete-if
	     (lambda (e)
	       (funcall (if compare-fn compare-fn #'equal) element e))
	     (eval list))))

;; Signal that RC has been loaded.
(provide 'rc)

;; Init package subsystem.
(requires packages)

;;; rc.el ends here

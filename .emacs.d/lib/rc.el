;;; rc --- Custom standard library.
;;; Commentary:
;;; Standard library that can always be assumed to be loaded when writing
;;; config files.
;;; Code:
(require 'cl-lib 'package)

;; Turn on lexical binding by default.
(setq lexical-binding t)

(defun clear-directory (path)
  "Clear the contents of PATH."
  (delete-directory path t)
  (make-directory path))

(defun move-key (keymap-from keymap-to key)
  "Move binding from KEYMAP-FROM to KEYMAP-TO, deleting KEY from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defmacro requires (&rest libs)
  "Let you require multiple LIBS without quoting them."
  `(mapc (symbol-function 'require)
	 (list ,@(mapcar (lambda (lib) `(quote ,lib)) libs))))

(defmacro provides (&rest libs)
  "Let you provide multiple LIBS without quoting them."
  `(mapc (symbol-function 'provide)
	 (list ,@(mapcar (lambda (lib) `(quote ,lib)) libs))))

(defun mklist (thing)
  "Ensure THING is a list."
  (if (listp thing)
      thing
    (list thing)))

(defun compiled-file-path (path)
  "Return the compiled file path for PATH."
  (format "%sc" path))

(cl-defmacro save-buffer-info (&rest body)
  "Saves information about the current buffer, runs body, and restores info."
  `(save-excursion
     (save-restriction
       ,@body)))

(defun buffer-starts-with (regex)
  "Return whether the current buffer start with REGEX."
  (save-buffer-info
   (widen)
   (goto-char (point-min))
   (save-match-data
     (looking-at regex))))

(defun buffer-is-script ()
  "Return whether the current buffer is a script."
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
  "Fully flatten TREE."
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
  (count-lines (point-min) (point-max)))

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

(defun autostart-programs ()
  "Starts all autostart programs."
  (start-process "autostart" "autostart" "/home/miningmarsh/.autostarts"))

(defun remove-from-list (list element &optional compare-fn)
  "Remove from LIST ELEMENT, using COMPARE-FN."
  (set list (cl-delete-if
	     (lambda (e)
	       (funcall (if compare-fn compare-fn #'equal) element e))
	     (eval list))))

(defmacro create-global-modes (&rest minor-modes)
  "Create global modes for given MINOR-MODES."
  `(progn
     ,@(mapcar
	(lambda (mode)
	  `(define-globalized-minor-mode
	     ,(->> mode
		   symbol-name
		   (format "global-%s")
		   intern)
	     ,mode
	     (lambda () (,mode 1))))
	minor-modes)))

(defmacro bind-global (&rest bindings)
  "Create global keybinds BINDINGS."
  `(progn
     ,@(mapcar
	(lambda (binding)
	  (bind-head-tail (key func) binding
			  `(global-set-key
			    ,key
			    (lambda ()
			      (interactive)
			      ,(car func)))))
	(partition 2 bindings))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).
Stolen from: http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html"
  (->> string
       (replace-regexp-in-string "[ \t\n]*\\'" "")
       (replace-regexp-in-string "\\`[ \t\n]*" "")))

(defun date ()
  "Print the current date/time as a message."
  (interactive)
  (message (current-time-string)))

(defmacro unless-setq (&rest sets)
  `(progn
     ,@(mapcar
	(lambda (entry)
	  `(unless ,(car entry) (setq ,(car entry) ,(cadr entry))))
	(partition 2 sets))))

(defun ! (command)
  "Run shell COMMAND, producing a message with leading and trailing whitespace stripped."
  (let ((result (-> command shell-command-to-string trim-string)))
    (when (and result (> (length result) 0))
      (message "%s" result))))

(lexical-let ((last-gcs-done 0)
	      (last-gc-elapsed 0)
	      (last-gc-average 0.0))
  (defun running-gc-time (&optional print-message)
    (interactive "p")
    "Return the current average time needed to perform a garbage collect."
    (when (< last-gcs-done gcs-done)
      (setq last-gc-average
	    (/ (- (float gc-elapsed) (float last-gc-elapsed))
	       (- (float gcs-done) (float last-gcs-done))))
      (setq last-gcs-done gcs-done
	    last-gc-elapsed gc-elapsed))
    (when print-message
      (message "%s" last-gc-average))
    last-gc-average))

(cl-defmacro defer-after-init (&body body)
  "Run BODY after init has finished."
  `(add-hooks (after-init-hook)
	      ,@body))

(defun launch-program (program-name)
  "Launch PROGRAM-NAME in a new buffer."
  (start-process-shell-command program-name nil program-name))

(defun remove-if-nil (list)
  "Remove every nil element from LIST."
  (cl-remove-if-not 'identity list))

(defun frame-window-list ()
  "Return list of windows in current frame."
  (->> (selected-frame)
       buffer-list
       (mapcar 'get-buffer-window)
       remove-if-nil))

(defun zip (&rest lists)
  "Zip LISTS together."
  (apply #'cl-mapcar #'list lists))

(defun swap-buffers (first second)
  "Swap the buffers contained by window FIRST and window SECOND."
  (let1 temp-buffer (window-buffer first)
	(set-window-buffer first (window-buffer second))
	(set-window-buffer second temp-buffer)))

(defun global-symbols ()
  "Return list of bound non-lexical global symbols."
  (loop
   for symbol being the symbols
   if (boundp symbol)
   collect symbol))

(defun bind-symbol (symbol value)
  "Set SYMBOL to VALUE."
  (interactive
   (list (intern (completing-read "Bind symbol: "
				  (global-symbols)
				  #'identity 'confirm))
	 (call-interactively 'eval-expression)))
  (set symbol value))

(defun retrieve-symbol (symbol)
  "Print value of SYMBOL."
  (interactive
   (list (intern (completing-read "Retrieve symbol: "
				  (global-symbols)
				  #'identity 'confirm))))
  (message "%s" (symbol-value symbol)))

(cl-defmacro when-let (symbol value &rest body)
  "Bind symbol and only execute body if symbol is true."
  `(let1 ,symbol ,value
	 (when ,symbol
	   ,@body)))

(cl-defmacro if-let (symbol value true false)
  "Bind SYMBOL to VALUE and execute TRUE if non-nil, FALSE otherwise."
  `(let1 ,symbol ,value
	 (if ,symbol
	     ,true
	   ,false)))

(cl-defmacro to-file (filename &rest body)
  "Write the value to FILENAME returned by BODY, then forward return value."
  (with-gensym result
	       `(let1 ,result (progn ,@body)
		      (with-temp-file ,filename
			(insert
			 (message "%s" ,result)))
		      ,result)))

(defun eval-file (file)
  "Execute FILE and return the result of the last expression."
  (eval
   (cdar
    (ignore-errors
      (read-from-string
       (format "(progn %s)"
	       (with-temp-buffer
		 (insert-file-contents file)
		 (buffer-string))))))))

(defun read-file (file)
  "Read FILE and return the list of expressions in the file."
  (cdar
   (ignore-errors
     (read-from-string
      (format "(list %s)"
	      (with-temp-buffer
		(insert-file-contents file)
		(buffer-string)))))))

(defun read-file-last (file)
  "Read FILE and return the last expression in the file."
  (-> file
      read-file
      last
      car))

(defun read-file-first (file)
  "Read FILE and return the first expression in the file."
  (-> file
      read-file
      car))

(defun config-file (filename)
  "Return the cconfig filename for FILENAME."
  (format "%s/.emacs.d/%s"
	  (getenv "HOME")
	  filename))

(cl-defun wall (arg fn &optional (compare-fn 'equal))
  "Repeatably pass ARG to FN.  Set ARG to the result of the next FN call.
Return ARG once the value from FN stops changing.  Results are compared
using COMPARE-FN."
  (let ((current arg)
	(run nil)
	(last nil))
    (while (or (not run)
	       (not (funcall compare-fn current last)))
      (setq run t)
      (setq last current)
      (setq current (funcall fn current)))
    current))

(defun trace (value)
  "Print VALUE to messages buffer, then return VALUE."
  (message "Trace: %s" value)
  value)

(cl-defmacro interactively (&rest body)
  "Create an interactive lambda on the BODY."
  `(lambda (&rest args)
     (interactive)
     ,@body))

(defun start-music ()
  "Start the freedom chant."
  (interactive)
  (start-process-shell-command
   "music"
   "music"
   "mplayer /home/miningmarsh/.emacs.d/stallman.mp3 -loop 0"))

(defun stop-music ()
  "Stop the freedome chant."
  (interactive)
  (kill-process "music"))

;; Signal that RC has been loaded.
(provide 'rc)

;; Init package subsystem.
(requires packages)


;;; rc.el ends here

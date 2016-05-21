;;; key-tree --- Key tree subsystem.
;;; Commentary:
;;; Code:
(require 'rc)

(defeat key-tree (evil evil-leader which-key)

  ;; Key for global vim binds.
  ;;(add-to-list 'exwm-input-prefix-keys (elt (kbd "M-<SPC>") 0))
  (unless (boundp 'meta-space-map)
    (define-prefix-command 'meta-space-map)
    (global-set-key (kbd "M-<SPC>") 'meta-space-map))

  ;; Use space as the leader.
  (evil-leader/set-leader "<SPC>")

  (defun key-tree/--map-key-tree (describe-key-fn key-fn key-tree)
    (let1 result nil
	  (cl-labels

	      ;; Set a key-tree recursively
	      ((set-key-tree
		(prefix description tree)

		;; While there are still keys to parse in the tree.
		(while tree

		  ;; Pop the next key, and the next description/subtree.
		  (let* ((key (cl-first tree))
			 (value (cl-second tree))
			 (new-tree (cddr tree)))
		    (setq tree new-tree)

		    ;; If we are dealing with a subtree, recurse.
		    (if (and (not (stringp value))
			     (listp value))

			;; Grab the description of the next key.
			(let* ((description (first value))
			       (new-value (cdr value)))
			  (setq value new-value)

			  ;; Add the key-description for the current subtree.
			  (setq result
				(nconc result
				       (funcall describe-key-fn
						(substring (concat prefix " " key) 1)
						description)))

			  ;; Recurse
			  (set-key-tree
			   ;; Append the next key to the prefix for the
			   ;; new tree.
			   (concat prefix " " key)
			   description
			   value))

		      ;; Otherwise, pop the command from the tree.
		      (let* ((command (first tree))
			     (new-tree (cdr tree)))
			(setq tree new-tree)
			;; Add the key description.
			(setq result
			      (nconc result
				     (funcall key-fn
					      (substring (concat prefix " " key) 1)
					      command)
				     (funcall describe-key-fn
					      (substring (concat prefix " " key) 1)
					      value)))))))))

	    ;; Recurse on the tree.
	    (set-key-tree "" "All" key-tree))
	  `(progn ,@result)))

  (cl-defmacro key-tree/add-bindings (&rest key-tree)
    "Add KEY-TREE to the binding set."
    (key-tree/--map-key-tree

     (lambda (key-sequence description)
       (list
	`(which-key-add-key-based-replacements
	   ,(concat "<SPC> " key-sequence) ,description
	   ,(concat "M-<SPC> " key-sequence) ,description)))

     (lambda (key-sequence command)
       (list
	`(global-set-key
	  (kbd ,(concat "M-<SPC> " key-sequence)) ,command)
	`(evil-leader/set-key ,(kbd key-sequence) ,command)))

     key-tree))

  (cl-defmacro key-tree/add-editor-bindings (&rest key-tree)
    "Add KEY-TREE to the binding set."
    (key-tree/--map-key-tree

     (lambda (key-sequence description)
       (list
	`(which-key-add-key-based-replacements
	   ,(concat "<SPC> " key-sequence) ,description)))

     (lambda (key-sequence command)
       (list
	`(evil-leader/set-key ,(kbd key-sequence) ,command)))

     key-tree))

  (cl-defmacro key-tree/add-mode-bindings (mode &rest key-tree)
    "Add to MODEs key binding set the set specified in KEY-TREE."
    (key-tree/--map-key-tree

     (lambda (key-sequence description)
       (list
	`(which-key-add-major-mode-key-based-replacements (quote ,mode)
	   ,(concat "<SPC> " key-sequence) ,description)))

     (lambda (key-sequence command)
       (list
	`(evil-leader/set-key-for-mode (quote ,mode)
	   ,(kbd key-sequence) ,command)))

     key-tree)))

;;; key-tree.el ends here

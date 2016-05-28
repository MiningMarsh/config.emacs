;;; calc --- Calculus commands.
;;; Commentary:
;;; Code:
(packages/define calc ()

  (defun resize-vector (size vector)
    (let1 len (length vector)
	  (if (<= size len)
	      (subseq vector 0 size)
	    (let1 new-vector (copy-list vector)
		  (while (not (= len size))
		    (setq len (1+ len))
		    (if (not new-vector)
			(setq new-vector (list 0))
		      (nconc new-vector (list 0))))
		  new-vector))))

  (defwrap + (&rest vals)
    (if (cl-reduce
	 (lambda (l r) (and l r))
	 (cl-mapcar #'listp vals))
	()))


  (defun resize-vectors (size &rest vectors)
    (mapcar (lambda (vector) (resize-vector size vector))
	    vectors))

  (defun normalize-vector-sizes (&rest vectors)
    (apply 'resize-vectors (apply 'max (mapcar 'length vectors))
	   vectors))

  (defun dot-product (&rest vectors)
    (->> vectors
	 (normalize-vector-sizes)
	 (apply 'cl-mapcar (lambda (args) (apply '* args)))
	 (reduce '+)))

  (defun vector-length (vector)
    (sqrt
     (cl-reduce (lambda (acc val)
		  (+ acc (* val val)))
		vector)))

  (defun scale-vector (scale-factor vector)
    (cl-mapcar (lambda (val)
		 (* val scale-factor))
	       vector))

  (defun scale-down-vector (scale-factor vector)
    (cl-mapcar (lambda (val)
		 (/ val scale-factor))
	       vector)))

(defun unit-vector-of (vector)
  (scale-down-vector (vector-length vector) vector))
;;; calc.el ends here

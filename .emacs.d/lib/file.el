;;; file --- File library.
;;; Commentary:
;;; Code:
(packages/define
    file ()

    (defun file/directory-of (file &optional interactive)
      "Return the directory a file is in.  Print the directory if interactive."
      (-> buffer-file-name
	  (list t)
	  interactive)

      (let1 directory (file-name-directory file)
	    (when interactive
	      (message "Directory of '%s' is: %s"
		       (file-name-nondirectory file)
		       directory)))))
;;; file.el ends here

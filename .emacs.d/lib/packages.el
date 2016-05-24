;;; packages --- Install packages that are needed by config automagically.
;;; Commentary:
;;; Code:
(require 'rc)

(requires cl cl-lib package)

;; Add package repositories.
(mapc (lambda (e) (add-to-list 'package-archives e))
      (assoc-map "melpa-stable" "http://melpa-stable.milkbox.net/packages/"
		 "marmalade" "http://marmalade-repo.org/packages/"
		 "gnu"       "http://elpa.gnu.org/packages/"
		 "melpa"     "http://melpa.milkbox.net/packages/"))

;; Init package subsystem.
(package-initialize)

(defvar packages/finished nil
  "Whether package-finish has yet been called.")

(defvar packages/installed 0
  "The number of packages installed on startup.")

(defvar packages/upgraded 0
  "The number of packages upgraded on startup.")

(defvar packages/removed 0
  "The number of packages that were removed on startup.")

(defun packages/-refresh-contents-if-not-already ()
  "Refresh package database if not already refreshed."
  (only-once (package-refresh-contents)))

;; Hook that updates a cache timestamp after updates.
(advice-add 'package-refresh-contents
	    :after
	    (lambda (&rest args)
	      ;; Print a timestamp to the cache file.
	      (to-file (config-file "last-updated")
		       (decode-time))))

(defun packages/-refresh-contents-if-needed (&optional interactive)
  "Refresh package contents if not already refreshed.  Print output if INTERACTIVE is non-nil."
  (interactive (list t))

  ;; Only refresh if we have never refreshed, or haven't refreshed for at least
  ;; a day.
  (if (or

       ;;First check if we ever refreshed.
       (not (-> "last-updated"
		config-file
		file-exists-p))

       ;; Second, check if the last update time was more than a day ago.
       (not (=  (-> (decode-time)
		    cl-fourth)
		(-> "last-updated"
		    config-file
		    read-file-last
		    cl-fourth))))

      ;; Refresh the package contents database.
      (with-temp-message "Refreshing package contents database..."
	(package-refresh-contents))

    (when interactive
      (message "Package contents do not need to be refreshed."))))

(defun packages/-find-feature-path (feature)
  "Return a file path if FEATURE could be found in the 'load-path'."
  (-> feature
      symbol-name
      locate-library))

(cl-defun packages/dependencies (orig-package &optional list-package list-builtins)
  "Returns a list of packages that PACKAGE depends on, including their dependencies.
If LIST-PACKAGE is non-nil, include the package in the results.
If LIST-BUILTINS is non-nil, include emacs builtin packages in the results."
  (lexical-let ((orig-package orig-package))
    (wall (list orig-package)
	  (lambda (packages)
	    (cl-remove-duplicates
	     (cl-remove-if
	      (lambda (p)
		(or
		 (unless list-package
		   (equal p orig-package))
		 (unless list-builtins
		   (assq p package--builtins))))

	      (append packages
		      (mappend
		       (lambda (package)
			 (when-let
			  package-desc (packages/-description package)
			  (when (package-desc-p package-desc)
			    (mapcar 'car (package-desc-reqs package-desc)))))
		       packages))))))))

(defvar packages/-marked nil
  "List of packages the user has marked.")

(defun packages/mark (package)
  "Mark PACKAGE as being requested to be installed by the user."
  (setq packages/-marked
	(cl-remove-duplicates
	 (append
	  packages/-marked
	  (packages/dependencies package t)))))

(defun packages/-uninstall-all-unmarked ()
  "Uninstall all unneeded packages."
  (mapc (lambda (p)
	  (package-uninstall p)
	  (setq packages/removed
		(1+ packages/removed)))
	(cl-remove-if
	 (lambda (p) (member p package-marked-list))
	 (mapcar 'car package-alist))))

(add-hook 'after-config-hook
	  'packages/-uninstall-all-unmarked)

(defun packages/-description-remote (package)
  "Return the description of PACKAGE if it is available remotely, nil otherwise."
  (-> package
      (assq package-archive-contents)
      cadr))

(defun packages/-description-local (package)
  "Return the description of PACKAGE if it is available locally, nil otherwise."
  (or (cadr (assq package package-alist))
      (cdr (assq package package--builtins))))

(cl-defmacro packages/-bind-descriptions ((old new) package &body body)
  "Bind old package description to OLD, new to NEW, and execute BODY."
  `(let ((,old (packages/-description-local ,package))
	 (,new (packages/-description-remote ,package)))
     ,@body))

(defun packages/-description (package)
  "Return the latest package description available for PACKAGE."
  (packages/-bind-descriptions (local remote) package
			     (or remote local)))

(defun packages/-latest-installed? (package &optional interactive)
  "Return whether the installed version of PACKAGE is up to date.  Print interactive status when INTERACTIVE is non-nil."
  (interactive (list
		(intern-soft
		 (completing-read "Check if package is up to date: "
				  (mapcar 'car package-alist)
				  'identity
				  t))
		t))

  ;; Grab the package descriptions.
  (packages/-bind-descriptions
   (package-local package-remote) package

   ;; Only perform the check if we have both a local and remote version
   (when (and package-remote
	      package-local
	      (package-desc-p package-remote)
	      (package-desc-p package-local))

     ;; Grab the package versions.
     (let ((package-version-remote (package-desc-version package-remote))
	   (package-version-local (package-desc-version package-local)))

       ;; Perform the version comparison.
       (if (or (version-list-= package-version-remote package-version-local)
	       (version-list-< package-version-remote package-version-local))
	   (progn
	     (when interactive
	       (message "Latest version of '%s' is installed." package))
	     t)
	 (progn
	   (when interactive
	     (message "Installed version of '%s' is out of date." package)
	     nil)))))))

(defun pn/uninstall-all-outdated (&optional interactive)
  "Delete all outdated packages.  Print output if INTERACTIVE is non-nil."
  (interactive (list t))
  (let1 outdated (->> package-alist
		      ;; Retrieves outdated packages.
		      (mapcar 'cddr)
		      remove-if-nil
		      (apply #'append))
	(when interactive
	  (if outdated
	      (message "Uninstalling %s outdated packages..."
		       (length outdated))
	    (message "No outdated packages found.")))
	(mapc 'package-delete outdated)))

(defun packages/uninstall-outdated (package &optional interactive)
  "Delete all outdated versions of PACKAGE that are installed.  Print interactive output if INTERACTIVE is non-nil."
  (interactive
   (if (->> package-alist
	    (mapcar 'cddr)
	    (cl-reduce (lambda (l r) (or l r))))
       (list
	(intern-soft
	 (completing-read "Uninstall outdated versions of package: "
			  (mapcar 'car package-alist)
			  (lambda (p) (cddr (assq p package-alist)))
			  t)
	 t))
     (list nil t)))
  (if (not package)
      (when interactive
	(message "No outdated packages found."))
    (if-let package-info (assq package package-alist)
	    (if-let packages (cddr package-info)
		    (progn
		      (when interactive
			(message "Uninstalled %s outdated versions of '%s'."
				 (length packages)
				 package))
		      (mapc 'package-delete packages))
		    (when interactive
		      (message "No outdated version of '%s' found." package)))

	    (when interactive
	      (message "Package '%s' not installed." package)))))

(defun packages/uninstall (package &optional interactive)
  "Uninstalls all versions of PACKAGE that are installed.  Print output when INTERACTIVE is non-nil."
  (interactive (list
		(intern-soft
		 (completing-read "Uninstall package: "
				  (mapcar 'car package-alist)
				  'identity
				  t))
		t))

  (if (not package)
      (when interactive
	(message "No package specified for uninstallation, aborting."))
    (if-let package-desc (assq package package-alist)
	    (progn
	      (mapc 'package-delete (cdr package-desc))
	      (when interactive
		(message "Package '%s' uninstalled." package)))
	    (when interactive
	      (message "Package '%s' was never installed." package)))))

(defun packages/upgrade (package &optional interactive)
  "Install the newest version of PACKAGE, uninstalling the old version.  Print output when INTERACTIVE is non-nil."
  (interactive (list
		(intern-soft
		 (when-let packages (->> package-alist
					 (mapcar 'car)
					 (cl-remove-if 'packages/-latest-installed?))
			   (completing-read "Upgrade package: "
					    packages
					    'identity
					    t)))
		t))

  (if (not package)
      (when interactive
	(message "No upgradable packages were found."))
    (packages/-bind-descriptions
     (local remote) package

     ;; Only execute if not up to date.
     (when (and local
		remote
		(package-desc-p local)
		(package-desc-p remote)
		(not (packages/-latest-installed? package)))

       ;; Install new package.
       (package-install-from-archive remote)

       ;; Uninstall outdated package.
       (packages/uninstall-outdated package)

       ;; Upgrade any dependencies needed.
       (mapc 'packages/-install-or-upgrade-if-needed (package-deps package))

       ;; Increment package upgrade count.
       (setq packages/upgraded
	     (1+ packages/upgraded))

       (when interactive
	 (message "Upgraded '%s'." package))

       ;; Return new version.
       (packages/-description-local package)))))

(defun packages/feature-exists (feature)
  "Return whether FEATURE exists either remotely or locally."
  (or (packages/-find-feature-path feature)
      (packages/-description-local feature)
      (packages/-description-remote feature)))

(defun packages/-upgrade-if-needed (package)
  "Upgrade PACKAGE if not already upgraded."
  (when (and (packages/feature-exists package)
	     (not (package-latest-p package)))
    (packages/upgrade package)))

(defun packages/-install-or-upgrade-if-needed (package)
  "Install PACKAGE if it is not already installed, otherwise upgrade PACKAGE if it is outdated."

  ;; Interactive prompt.
  (-> (completing-read "Install or upgrade package: "
		       (->> package-alist
			    (append package-archive-contents)
			    (mapcar 'car)
			    cl-remove-duplicates)
		       'identity t)
      intern-soft
      list
      interactive)

  ;; Actually perform the upgrade/install.
  (when (packages/feature-exists package)
    (if (and (packages/-description-remote package)
	     (not (package-installed-p package)))
	(progn
	  (setq packages/installed
		(1+ packages/installed))
	  (package-install package))
      (packages/-upgrade-if-needed package))))

(defmacro packages/requires (libs &rest body)
  "Let you require multiple LIBS things without quoting them.
Only run BODY if they could be loaded."
  `(when (and
	  ,@(mapcar
	     (lambda (lib)
	       `(progn

		  ;; If the package is not available remotely or locally, spit
		  ;; an error message.
		  (if (not (or (packages/feature-exists (quote ,lib))
			       (packages/-refresh-contents-if-not-already)
			       (packages/feature-exists (quote ,lib))))
		      (progn
			(message "Could not install package/load feature '%s'" (quote ,lib))
			nil)
		    (progn
		      (packages/-refresh-contents-if-needed)
		      (packages/-install-or-upgrade-if-needed (quote ,lib))
		      (packages/mark (quote ,lib))
		      (if ,(string-match ".*-theme" (symbol-name lib))
			  t
			(require (quote ,lib) nil 'noerror))))))
	     libs))
     ,@body))

(defmacro packages/define (feature-name deps &rest body)
  "Define a feature FEATURE-NAME that depends on DEPS, with code BODY."
  `(packages/require ,deps
	      ,@body
	      (provides ,feature-name)))

(provides packages)
;;; packages.el ends here

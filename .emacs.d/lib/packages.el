;;; packages --- Install packages that are needed by config automagically.
;;; Commentary:
;;; Code:
(require 'rc)

(requires cl-lib package)

;; Add package repositories.
(mapc (lambda (e) (add-to-list 'package-archives e))
      (assoc-map "melpa-stable" "http://melpa-stable.milkbox.net/packages/"
		 "marmalade" "http://marmalade-repo.org/packages/"
		 "gnu"       "http://elpa.gnu.org/packages/"
		 "melpa"     "http://melpa.milkbox.net/packages/"))

;; Init package subsystem.
(package-initialize)

;; Make package-refresh-contents update the last updated cache that
;; package-refresh-contents-if-needed uses.
(advice-add 'package-refresh-contents
	    :after
	    (lambda (&rest args)

	      ;; Print a timestamp to the cache file.
	      (to-file (config-file "last-updated")
		       (decode-time))))

(defun package-refresh-contents-if-needed (&optional interactive)
  "Refresh package contents if not already refreshed.  Print output if INTERACTIVE is non-nil."
  (interactive (list t))

  ;; Only refresh if we have never refreshed, or haven't refreshed for at least
  ;; a day. First check if we ever refreshed.
  (if (or (not (-> "last-updated"
		   config-file
		   file-exists-p))
	  ;; Second, check if the last update time was more than a day ago.
	  (not (=  (-> (decode-time)
		       fourth)
		   (-> "last-updated"
		       config-file
		       read-file-last
		       fourth))))

      ;; Refresh the package contents database.
      (with-temp-message "Refreshing package contents database..."
	(package-refresh-contents))

    (when interactive
      (message "Package contents do not need to be refreshed."))))

(defun find-feature-path (feature)
  "Return a file path if FEATURE could be found in the 'load-path'."
  (-> feature
      symbol-name
      locate-library))

(defun package-description-remote (package)
  "Return the description of PACKAGE if it is available remotely, nil otherwise."
  (package-refresh-contents-if-needed)
  (-> package
      (assq package-archive-contents)
      cadr))

(defun package-description-local (package)
  "Return the description of PACKAGE if it is available locally, nil otherwise."
  (or (cadr (assq package package-alist))
      (cdr (assq package package--builtins))))

(cl-defmacro bind-package-descriptions ((old new) package &body body)
  "Bind old package description to OLD, new to NEW, and execute BODY."
  `(let ((,old (package-description-local ,package))
	 (,new (package-description-remote ,package)))
     ,@body))

(defun package-description (package)
  "Return the latest package description available for PACKAGE."
  (bind-package-descriptions (local remote) package
			     (or remote local)))

(defun package-latest-p (package &optional interactive)
  "Return whether the installed version of PACKAGE is up to date.  Print interactive status when INTERACTIVE is non-nil."
  (interactive (list
		(intern-soft
		 (completing-read "Check if package is up to date: "
				  (mapcar 'car package-alist)
				  'identity
				  t))
		t))

  ;; Grab the package descriptions.
  (bind-package-descriptions
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

(defun package-uninstall-all-outdated (&optional interactive)
  "Delete all outdated packages.  Print output if INTERACTIVE is non-nil."
  (interactive (list t))
  (let1 outdated (->> package-alist
		      ;; Retrieves outdated packages.
		      (mapcar 'cddr)
		      (remove-if-nil)
		      (apply #'append))
	(when interactive
	  (if outdated
	      (message "Uninstalling %s outdated packages..."
		       (length outdated))
	    (message "No outdated packages found.")))
	(mapc 'package-delete outdated)))

(defun package-uninstall-outdated (package &optional interactive)
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

(defun package-uninstall (package &optional interactive)
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

(defun package-upgrade (package &optional interactive)
  "Install the newest version of PACKAGE, uninstalling the old version.  Print output when INTERACTIVE is non-nil."
  (interactive (list
		(intern-soft
		 (when-let packages (->> package-alist
					 (mapcar 'car)
					 (cl-remove-if 'package-latest-p))
			   (completing-read "Upgrade package: "
					    packages
					    'identity
					    t)))
		t))

  (if (not package)
      (when interactive
	(message "No upgradable packages were found."))
    (bind-package-descriptions
     (local remote) package

     ;; Only execute if not up to date.
     (when (and local
		remote
		(package-desc-p local)
		(package-desc-p remote)
		(not (package-latest-p package)))

       ;; Install new package.
       (package-install-from-archive remote)

       ;; Uninstall outdated package.
       (package-uninstall-outdated package)

       (when interactive
	 (message "Upgraded '%s'." package))

       ;; Return new version.
       (package-description-local package)))))

(defun feature-exists (feature)
  "Return whether FEATURE exists either remotely or locally."
  (or (find-feature-path feature)
      (package-description-local feature)
      (package-description-remote feature)))

(defun package-upgrade-if-needed (package)
  "Upgrade PACKAGE if not already upgraded."
  (when (and (feature-exists package)
	     (not (package-latest-p package)))
    (package-upgrade package)))

(defun package-install-or-upgrade-if-needed (package)
  "Install PACKAGE if it is not already installed, otherwise upgrade PACKAGE if it is outdated."
  (interactive
   (->>
    (let1 packages
	  (->> package-alist
	       (append package-archive-contents)
	       (mapcar 'car)
	       cl-remove-duplicates)
	  (completing-read "Install or upgrade package: " packages 'identity t))
    intern-soft
    list))

  (when (feature-exists package)
    (if (and (package-description-remote package)
	     (not (package-installed-p package)))
	(package-install package)
      (package-upgrade-if-needed package))))

(defmacro requiring (libs &rest body)
  "Let you require multiple LIBS things without quoting them.
Only run BODY if they could be loaded."
  `(when (and
	  ,@(mapcar
	     (lambda (lib)
	       `(progn

		  ;; If the package is not available remotely or locally, spit
		  ;; an error message.
		  (if (not (feature-exists (quote ,lib)))
		      (progn
			(message "Could not install package/load feature '%s'" (quote ,lib))
			nil)
		    (progn
		      (package-install-or-upgrade-if-needed (quote ,lib))
		      (require (quote ,lib) nil 'noerror)))))
	     libs))
     ,@body))

(defmacro defeat (feature-name deps &rest body)
  "Define a feature FEATURE-NAME that depends on DEPS, with code BODY."
  `(requiring ,deps
	      ,@body
	      (provides ,feature-name)))

(provides packages)
;;; packages.el ends here

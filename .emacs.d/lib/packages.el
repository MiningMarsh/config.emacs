;;; packages --- Install packages that are needed by config automagically.
;;; Commentary:
;;; Code:
(require 'rc)

(requires cl-lib package)

;; Add package repositories.
(mapc (lambda (e) (push e package-archives))
      (assoc-map "marmalade" "http://marmalade-repo.org/packages/"
		 "melpa"     "http://melpa.milkbox.net/packages/"
		 "gnu"       "http://elpa.gnu.org/packages/"))

;; Init package subsystem.
(package-initialize)

(defvar requiring-already-tried nil
  "List of packages requiring has already tried to install.")

(lexical-let ((already-refreshed nil))
  (defun package-refresh-if-needed ()
    "Refresh package contents if not already refreshed."
    (when (not already-refreshed)
      (setq already-refreshed t)
      (with-temp-message "Refreshing package contents database..."
	(package-refresh-contents)))))

(defun find-feature-path (feature)
  "Return a file path if FEATURE could be found in the 'load-path'."
  (-> feature
      symbol-name
      locate-library))

(defun package-description-remote (package)
  "Return the description of PACKAGE if it is available remotely, nil otherwise."
  (package-refresh-if-needed)
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

(defun package-latest-p (package)
  "Return whether the installed version of PACKAGE is up to date."

  ;; Grab the package descriptions.
  (bind-package-descriptions
   (package-local package-remote) package

    ;; Only perform the check if we have both a local and remote version
    (when (and package-remote package-local)

      ;; Grab the package versions.
      (let ((package-version-remote (package-desc-version package-remote))
	    (package-version-local (package-desc-version package-local)))

	;; Perform the version comparison.
	(or (version-list-= package-version-remote package-version-local)
	    (version-list-< package-version-remote package-version-local))))))


(defun package-upgrade (package)
  "Install the newest version of PACKAGE, uninstalling the old version."
  (bind-package-descriptions
   (local remote) package

   ;; Only execute if not up to date.
   (when (and local remote (not (package-latest-p package)))

     ;; Install new package.
     (package-install-from-archive remote)

     ;; Uninstall outdated package.
     (package-delete local)

     ;; Return new version.
     (package-description-local package))))

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

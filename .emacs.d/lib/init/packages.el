;;; packages --- Install packages that are needed by config automagically.
;;; Commentary:
;;; Stolen almost verbatim from Bryan, tweaked to use rc.el stuff.
;;; Code:
(require 'rc)
(defeat packages (cl package)

  ;; Add package repositories.
  (mapc (lambda (e) (push e package-archives))
	(assoc-map "marmalade" "http://marmalade-repo.org/packages/"
		   "melpa"     "http://melpa.milkbox.net/packages/"
		   "gnu"       "http://elpa.gnu.org/packages/"))

  ;; Init package subsystem.
  (package-initialize)

  ;; The packages I want.
  (let1 packages
	'(elpy
	  evil
	  evil-leader
	  nyan-mode
	  evil-org
	  evil-paredit
	  flx-ido
	  flycheck
	  relative-line-numbers
	  key-chord
	  rainbow-delimiters
	  xclip
	  color-theme
	  rudel
	  slime
	  smart-mode-line
	  smartparens)

	;; Actually load those packages.
	(with-ignored-errors
	 (let1 refresh t
	       (dolist (p packages)
		 (when (not (package-installed-p p))
		   (when refresh
		     (setq refresh nil)
		     (package-refresh-contents))
		   (package-install p))))))

  ;; Manually load some packages.
  (load (expand-file-name "~/.quicklisp/slime-helper.el")))
;;; packages.el ends here

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
	'(color-theme
	  elpy
	  erc-image
	  erc-yt
	  evil
	  evil-leader
	  evil-org
	  evil-paredit
	  flx-ido
	  flycheck
	  jabber
	  key-chord
	  nyan-mode
	  rainbow-delimiters
	  relative-line-numbers
	  rudel
	  slime
	  smart-mode-line
	  smart-mode-line-powerline-theme
	  smartparens
	  xclip)

	;; Actually load those packages.
	(let1 refresh t
	      (dolist (p packages)
		(when (not (package-installed-p p))
		  (when refresh
		    (setq refresh nil)
		    (package-refresh-contents))
		  (with-ignored-errors (package-install p)))))))
;;; packages.el ends here

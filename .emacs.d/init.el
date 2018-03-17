;;; init --- Bootstraps emacs instance.
;;; Commentary:
;;; Load bootstrap library.
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file "~/.emacs.d/bootstrap/bootstrap.el")
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode window-number solarized-theme spaceline spacemacs-theme rainbow-delimiters smartparens slime flymake-puppet puppet-mode ag projectile magit smex ido-yes-or-no ido-ubiquitous ido-grid-mode flx-ido flycheck evil-paredit ranger erc-yt ac-ispell erc-image column-enforce-mode key-chord elpy pretty-symbols paredit ace-jump-mode dictionary which-key evil-leader evil function-args yasnippet auto-complete achievements))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "brightyellow"))))
 '(erc-my-nick-face ((t (:foreground "brightyellow" :weight bold)))))

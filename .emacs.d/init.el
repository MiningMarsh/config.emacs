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
    (markdown-preview-mode typescript-mode simpleclip evil-paredit evil yaml-mode window-number which-key spacemacs-theme spaceline solarized-theme smex smartparens slime ranger rainbow-delimiters puppet-mode projectile pretty-symbols paredit magit key-chord ido-yes-or-no ido-grid-mode function-args flymake-puppet flycheck flx-ido erc-yt erc-image elpy dictionary column-enforce-mode achievements ace-jump-mode ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "brightyellow"))))
 '(erc-my-nick-face ((t (:foreground "brightyellow" :weight bold)))))

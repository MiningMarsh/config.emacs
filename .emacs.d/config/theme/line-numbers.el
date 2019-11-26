;;; line-numbers --- Enable line numbers.
;;; Commentary:
;;; Code:
(packages/requires (linum linum-relative)

;;(defvar my-linum-current-line-number 0)
;;(setq linum-format 'my-linum-relative-line-numbers)
;;(defun my-linum-relative-line-numbers (line-number)
  ;;(let ((test2 (1+ (- line-number my-linum-current-line-number))))
    ;;(concat (propertize (format "%3d"  (cond ((<= test2 0) (1- test2))
                                             ;;((= test2 1) line-number)
                                             ;;((> test2 0) test2)))
                        ;;'face `(linum (:inverse-video ,(= test2 1))))
            ;;(propertize "" 'face `(linum (:inverse-video ,t))))))
;;(defadvice linum-update (around my-linum-update)
  ;;(let ((my-linum-current-line-number (line-number-at-pos)))
    ;ad-do-it))
;;(ad-activate 'linum-update)

  (defvar evil/line-numbers 't)



  (add-hooks (evil-motion-state-entry
    evil-operator-state-entry)
    (linum-mode 0)
    (linum-relative-mode (if evil/line-numbers 1 -1)))

  (add-hooks (evil-motion-state-exit
      evil-operator-state-exit)
      (linum-relative-mode -1)
      (linum-mode (if evil/line-numbers 1 0)))

  (linum-relative-mode 0)
  (linum-mode 1)
  (global-linum-mode 1)
  (custom-set-faces
    '(linum ((t (:background "gray16" :foreground "gold4" :underline nil :weight thin)))))

  ;; Set the funtion we use to format our number in absolute mode.
  (setq linum-format (lambda (line)
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
  (propertize (format (format " %%%dd " w) line) 'face 'linum)))))
;;; line-numbers.el ends here

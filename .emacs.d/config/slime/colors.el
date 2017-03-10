;;; colors --- Enable correct syntax coloring in slime swank prompts
;;; Commentary:
;;; Taken from:
;;; http://stackoverflow.com/questions/25809493/how-can-i-get-syntax-highlighting-for-common-lisp-in-slimes-repl
;;; Code:

;; No idea what this curerntly does.
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)

(add-hooks (slime-repl-mode-hook)
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  "Adivce used to correct colors on SLIME swank repl."
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))
;;; colors.el ends here

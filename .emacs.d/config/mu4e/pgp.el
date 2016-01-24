(requiring (mu4e epg-config)

(setq mml2015-use 'epg
      epg-user-id "joshua.songy@gmail.com"
      mml2015-encrypt-to-self t
      mml2015-sign-with-sender t)

(eval-after-load 'mu4e
  '(define-key mu4e-view-mode-map (kbd "C-c C-d") 'epa-mail-decrypt)))

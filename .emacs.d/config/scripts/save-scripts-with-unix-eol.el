; Makes sure that scripts are saved with a unix EOL regardless of OS. This is
; because cygwin emacs defaults to windows EOL.
(add-hooks (before-save-hook)
  (when (buffer-is-script)
    (set-buffer-file-coding-system 'emacs-mule-unix)))

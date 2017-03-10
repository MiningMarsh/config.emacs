;;; message-filter --- Allows one to filter messages.
;;; Commentary:
;;; This package can be used to rewrite or surpress Emacs messages.
;;; Code:
(packages/define message-filter ()

  (defvar message-filter/filters '()
    "Assoc-map of trigger to replacement functions for messages.")

  (defun message-filter/blacklist (&rest tests)
    "Blacklist any message that matches one of the given TESTS."
    (dolist (test tests)
      (setq message-filter/filters
        (cons
          (cons test nil)
          message-filter/filters))))

  ;; Wrap message to not display messages in the blacklist.
  (defwrap message (format-string &rest args)

    ;; If no format string was provided, passthrough nil.
    (if (not format-string)

      ;; Make sure not to forget to pass the extra arguments through.
      (apply #'message (const nil args))

      ;; Create the formatted string we want to message, ahead of time, so that
      ;; we can test if it is blacklisted. As well, keep an accumulator of
      ;; changes.
      (let result (apply 'format (cons format-string args))

        ;; Loop through every filter we want to test.
        (dolist (filter-map message-filter/filters)

          ;; Get the test predicate and the replacement mapping function.
          (bind-head-tail (filter transform) filter-map

            ;; Store the original filter and transformer so that we can
            ;; Map the user specified ones if they are not lambdas.
            (lexical-let ((original-filter filter)
                          (original-transform transform))

              ;; If the filter is a string, we treat it as a regex.
              (when (stringp filter)

                ;; If the transformer is also a string, use it to replace
                ;; everything the original regex matched.
                (when (and transform (stringp transform))
                  (setq transform
                    (lambda (string)
                      (replace-regexp-in-string
                        original-filter original-transform string))))

                ;; Replace the filter with the regex matching predicate.
                (setq filter
                  (lambda (string)
                    (string-match
                      original-filter
                      string)))))

            ;; If the transform function is a value, then we just make
            ;; it a mapping function that always returns that value.
            (when (not (functionp transform))

              ;; Store the original transform value so we can refer to
              ;; it.
              (lexical-let ((original-transform transform))
                (setq transform (lambda (test) original-transform))))

             ;; Actually apply the filter to see if the message was blacklisted.
             (when (funcall filter result)

               ;; If the message was blacklisted, transform it.
               (setq result (funcall transform result)))

               ;; Turn result into a string if it is not nil.
               (when (and result (not (stringp result)))
                 (setq result (format "%s" result)))))

               (if (not result)
                 (message nil)
                 (message "%s" result))))))
;;; message-filter.el ends here

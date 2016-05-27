;;; os --- Operating System Library.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/define os ()

  (defvar os/mac :mac
    "The symbol used to represent the OS X operating system.")

  (defvar os/windows :windows
    "The symbol used to represent the OS X operating system.")

  (defvar os/linux :linux
    "The symbol used to represent the OS X operating system.")

  (defvar os/type (cond
		   ((string-equal system-type "windows-nt") os/windows)
		   ((string-equal system-type "darwin") os/mac)
		   ((string-equal system-type "gnu/linux") os/linux))
    "Easier to use version of SYSTEM-TYPE.")

  (defun os/mac? ()
    (equal os/type os/mac))

  (defun os/linux? ()
    (equal os/type os/linux))

  (defun os/windows? ()
    (equal os/type os/windows))

  (when (os/mac?)
    (packages/requires (mac)))

  (cl-defmacro os/when-mac (&rest body)
    `(when (os/mac?)
       ,@body)))
;;; os.el ends here

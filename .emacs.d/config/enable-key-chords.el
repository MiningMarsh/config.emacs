;;; enable-key-chords --- Enable key chording globally.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (key-chord)
  (key-chord-mode 1))
;;; enable-key-chords.el ends here

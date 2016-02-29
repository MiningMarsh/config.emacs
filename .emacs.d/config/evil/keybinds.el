;;; keybinds --- Set default evil bindigs.
;;; Commentary:
;;; This includes global evil keybindings.
;;; Code:
(require 'rc)

(requiring (evil evil-leader key-chord mu4e ranger)

		   ;; Use space as the leader.
		   (evil-leader/set-leader "<SPC>")

		   ;; Free up space and ret in normal mode.
		   (move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
		   (move-key evil-motion-state-map evil-normal-state-map " ")

		   ;; Keys to apply to all buffers.
		   (evil-leader/set-key
			 "f" 'find-file
			 "b" 'switch-to-buffer
			 "k" 'kill-buffer
			 "F" 'deer))
;;; keybinds.el ends here

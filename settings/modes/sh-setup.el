;; ----------------------------------------------------------------------------
;; SH Mode Setup
;; ----------------------------------------------------------------------------
(provide 'sh-setup)
(require 'interactive-shell)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'sh-mode-hook 'my-sh-mode-hook) 
(defun my-sh-mode-hook ()
  (auto-fill-mode nil))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(defun my-shell-mode-hook ()
  (auto-fill-mode 0)
  (text-scale-set -1.1)

  (auto-complete-mode t)

  (setq ansi-color-for-comint-mode              t
        comint-scroll-to-bottom-on-input        t
        comint-scroll-to-bottom-on-output       t
        comint-move-point-for-output            t
        comint-prompt-read-only                 t))


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun shell-eval ()
  "Evaluates Shell commands in a script"
  (interactive) 
  (if (and transient-mark-mode mark-active)
      (shell-eval-region (region-beginning) (region-end))
    ;; May want to change this to paragraph depending on style of use
    (shell-eval-line-and-step)))

(defun switch-frame-next-shell ()
  "Switch to next shell buffer." 
  (interactive)
  (switch-frame-next-buffer '("\\*shell") '("^ "))
  (end-of-buffer))

(defun switch-frame-previous-shell ()
  "Switch to previous shell buffer." 
  (interactive)
  (switch-frame-previous-buffer '("\\*shell") '("^ "))
  (end-of-buffer))

(defun switch-frame-current-shell ()
  "Displays the current associated shell buffer."
  (interactive)
  (if current-shell-buffer
      (display-buffer current-shell-buffer)
    (shell))
  (end-of-buffer))

(defun man-at-point ()
  "Loads MAN page for whatever is at point"
  (interactive)
    (save-excursion
      (unless (region-active-p)
	(er/mark-method-call))
      (man (buffer-substring-no-properties
		    (region-beginning) (region-end)))))




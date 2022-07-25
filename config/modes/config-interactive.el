;; LISP

(defun config-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'config-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)



(defun elisp-eval ()
  "Evaluate a elisp defun or region and step"
  (interactive)
  (if (and transient-mark-mode mark-active)
	 (call-interactively     'eval-region)
    (call-interactively       'eval-defun))
  (deactivate-mark)    
  (end-of-defun))

(defun switch-frame-next-message ()
  "Cycle through message and backtrace buffers."
  (interactive)
  (switch-frame-next-buffer '("\\*Message" "\\*Backtrace") '("^ "))
  (goto-char (point-max)))

(defun switch-frame-current-message ()
  "Switch to current message buffer and move cursor to the end."
  (interactive)
  (with-current-buffer "*Messages*"
    (end-of-buffer-all))
  (display-buffer "*Messages*"))

(defun describe-variable-or-function ()
  "Combines `describe-variable', `describe-function', and `describe-face' into one.  Additionally does this instantaneously and applies display-*Help*-frame (correct formatting)."
  (interactive)
  (let ((calling-frame (get-frame))
	   fn)

    (cond
	((not (eq 0 (setq fn (variable-at-point))))
	 (describe-variable fn))
	((setq fn (function-called-at-point))
	 (describe-function fn))
	((setq fn (face-at-point t))
	 (describe-face fn))
	((message "No variable, function, or face at point")))    
    
    (raise-frame calling-frame)))	; Switch back to calling frame



;; ------------------------------------------------------------------------- ;;





(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

;; ----------------------------------------------------------------------------
;; LISP Mode Setup
;; ----------------------------------------------------------------------------
(provide 'lisp-setup)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook		'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook			'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook	'my-lisp-mode-hook)

(defun my-lisp-mode-hook ()
  (hs-minor-mode)
  (hs-hide-all)
  (flyspell-prog-mode)
  (turn-on-auto-fill)
  
  (add-to-list 'ac-sources 'ac-source-functions)
  (add-to-list 'ac-sources 'ac-source-variables)
  )

;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun elisp-eval ()
  "Evaluate a elisp defun or region and step"
  (interactive)
  (let ((eval-region-function 'eval-region)
        (eval-other-function 'eval-defun)
        (step-next-line-on-eval 1))
    
    (if (and transient-mark-mode mark-active)
        (call-interactively     eval-region-function)
      (call-interactively       eval-other-function))
    
    (deactivate-mark)
    (next-line step-next-line-on-eval)))

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
  "Combines describe-variable and describe-function into one.  Additionally does this instantaneously and applies display-*Help*-frame (correct formatting)."
  (interactive)
  (let ((calling-frame (get-frame))
	(fn (function-called-at-point))
	(v (variable-at-point)))
    (if (eq v 0)
	(describe-function fn)
      (describe-variable v))
    (raise-frame calling-frame)))	; Switch back to calling frame


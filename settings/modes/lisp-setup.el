;; ----------------------------------------------------------------------------
;; LISP Mode Setup
;; ----------------------------------------------------------------------------
(provide 'lisp-setup)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(setq emacs-lisp-mode-hook		'my-lisp-mode-hook
      lisp-mode-hook			'my-lisp-mode-hook
      lisp-interaction-mode-hook	'my-lisp-mode-hook)
(defun my-lisp-mode-hook ()
  (hs-minor-mode t)
  (hs-hide-all)
  (flyspell-prog-mode)

  (lambda-mode 1)
  
  (add-to-list 'ac-sources 'ac-source-functions)
  (add-to-list 'ac-sources ac-source-variables)
  (add-to-list 'ac-sources ac-source-symbols)
  ;; -------------------- Aesthetics ----------------------
  (font-lock-add-keywords
   nil
   '(("[0-9]+"                        ; Put near end
      .
      'font-lock-number-face)))
  
  ;; -------------------- Key bindings --------------------
  (local-set-many-keys
   ;; ---------- Evaluation ----------
   [(shift return)]     'elisp-eval
   
   ;; ---------- Completion ----------
   (kbd "<tab>")        'completion-at-point

   ;; ---------- Help ----------
   "\C-hf"      	'describe-variable-or-function
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "emacs "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "emacs "))

   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-current-message

   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe))

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
  (let ((fn (function-called-at-point))
	(v (variable-at-point))
	(b-exist (get-buffer "*Help*")))
    (if (eq v 0)
	(describe-function fn)
      (describe-variable v))
    (when b-exist
      (display-*Help*-frame "*Help*"))))
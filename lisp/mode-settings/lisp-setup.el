;; ----------------------------------------------------------------------------
;; LISP Mode Setup
;; ----------------------------------------------------------------------------
(provide 'lisp-setup)
(message ">>> lisp-setup - start")
;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook		'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook			'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook	'my-lisp-mode-hook)

(defun my-lisp-mode-hook ()
  (message ">>> lisp-hook")
  (hs-minor-mode)
  (hs-hide-all)
  (flyspell-prog-mode)
  (turn-on-auto-fill)


  ;; (add-to-list 'ac-sources 'ac-source-functions)
  ;; (add-to-list 'ac-sources 'ac-source-variables)
  ) 

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys		emacs-lisp-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]  'elisp-eval
  
  ;; ---------- Indent / Tabs ----------
  (kbd "<tab>")	'indent-for-tab-command   

  ;; ---------- Help ----------
  "\C-hf"			'describe-variable-or-function
  "\C-hv"			'describe-variable-or-function
  [(f1)]			'(lambda ()
				   (interactive)
				   (google-query-at-point t "emacs "))
  [(S-f1)]		'(lambda ()
				   (interactive)
				   (google-query-at-point nil "emacs "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "emacs "))

  ;; ---------- Frame Switching ----------
  [(f12)]           'switch-frame-current-message
  ;; [(S-f12)]           'ielm

  )


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
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
	((setq fn (face-at-point t))
	 (describe-face fn))
	((not (eq 0 (setq fn (variable-at-point))))
	 (describe-variable fn))
	((setq fn (function-called-at-point))
	 (describe-function fn))
	((message "No face, variable, or function at point")))    
    
    (raise-frame calling-frame)))	; Switch back to calling frame

;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting
;; ------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'emacs-lisp-mode        
 '(("[\\<-][0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("[/!<>]=\\|[<>]\\|\\(\\b\\(and\\|or\\|not\\|eq[1]?\\)\\b\\)"
     .
     'font-lock-relation-operator-face)))

(message ">>> lisp-setup - end")

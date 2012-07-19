;; ----------------------------------------------------------------------------
;; LISP Mode Setup
;; ----------------------------------------------------------------------------
(provide 'lisp-setup)

;; --------------------------------------------------------------------------
;; Multi-Window
;; --------------------------------------------------------------------------
(add-to-list
 'special-display-buffer-names
 (list "*Messages*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 50)
             '(width . 80)
             '(top . 30)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 0)))))

(add-to-list
 'special-display-buffer-names
 (list "*Backtrace*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 30)
             '(width . 80)
             '(top . 50)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 0)))))


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

  ;; -------------------- Aesthetics ----------------------
  (font-lock-add-keywords
   nil
   '(("[0-9]+"                        ; Put near end
      .
      'font-lock-number-face)))
  
  ;; -------------------- Key bindings --------------------
  (local-set-many-keys
   (kbd "<tab>")        'completion-at-point
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   
   [(f12)]              (ti::definteractive (display-buffer "*Messages*"))
   [(shift return)]     'elisp-eval))

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
  (switch-frame-next-buffer '("\\*Message" "\\*Backtrace") '("^ ")))


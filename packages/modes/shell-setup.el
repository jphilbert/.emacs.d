;; ----------------------------------------------------------------------------
;; SHELL / BATCH Mode Setup
;; ----------------------------------------------------------------------------
(provide 'shell-setup)

(require 'interactive-shell nil t)
(require 'ntcmd nil t) 


;; --------------------------------------------------------------------------
;; Multi-Window
;; --------------------------------------------------------------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*shell.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 50)
             '(width . 80)
             '(top . 40)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 60)))))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bat\\'" . ntcmd-mode))
(add-hook 'ntcmd-mode-hook 'my-bat-mode-hook)
(defun my-bat-mode-hook ()
  (auto-fill-mode nil)
  (auto-complete-mode t)

  ;; -------------------- Key bindings -------------------- 
  (local-set-many-keys
   [(shift return)]     'shell-eval
   
   [(f12)]              'switch-frame-current-shell
   [S-f12]              'shell-new
   [C-f12]              'shell-buffer-choose
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(defun my-shell-mode-hook ()
  (auto-fill-mode nil)
  (auto-complete-mode t)
  (setq ansi-color-for-comint-mode              t
        comint-scroll-to-bottom-on-input        t
        comint-scroll-to-bottom-on-output       t
        comint-move-point-for-output            t
        comint-prompt-read-only                 t)
  
  ;; -------------------- Key bindings -------------------- 
  (local-set-many-keys
   [(f12)]              'switch-frame-next-shell
   [S-f12]              'shell-new
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))


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
  (switch-frame-next-buffer '("\\*shell") '("^ ")))

(defun switch-frame-previous-shell ()
  "Switch to previous shell buffer." 
  (interactive)
  (switch-frame-previous-buffer '("\\*shell") '("^ ")))

(defun switch-frame-current-shell ()
  "Displays the current associated shell buffer."
  (interactive)
  (if current-shell-buffer
      (display-buffer current-shell-buffer)
    (shell)))

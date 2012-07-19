;; ----------------------------------------------------------------------------
;; SHELL Mode Setup
;; ----------------------------------------------------------------------------
(provide 'powershell-setup)

(require 'interactive-shell nil t)
(autoload 'powershell "powershell"
  "Start a interactive shell of PowerShell." t)
(autoload 'powershell-mode "powershell-mode"
  "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

;; --------------------------------------------------------------------------
;; Multi-Window
;; --------------------------------------------------------------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*PowerShell.*\\*.*" 'display-default-frame
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
(add-hook 'powershell-mode-hook 'my-powershell-mode-hook)
(defun my-powershell-mode-hook ()
  (auto-fill-mode nil)
  (auto-complete-mode t)
  (setq shell-buffer-search-string "*PowerShell")

  ;; -------------------- Key bindings -------------------- 
  (local-set-many-keys
   [(shift return)]     'shell-eval
   
   [(f12)]              'switch-frame-current-powershell
   [S-f12]              (ti::definteractive
                         (powershell (generate-new-buffer-name
                                      "*PowerShell*")))
   [C-f12]              'shell-buffer-choose
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))

(add-hook 'powershell-launch-hook 'my-powershell-hook)
(defun my-powershell-hook ()
  ;; NOTE: Power Shell runs the shell hook (first) so anything done there may
  ;;  need to be undone here
  ;; (auto-complete-mode t)
  (setq ansi-color-for-comint-mode              t
        comint-scroll-to-bottom-on-input        t
        comint-scroll-to-bottom-on-output       t
        comint-move-point-for-output            t
        comint-prompt-read-only                 t)
  
  ;; -------------------- Key bindings --------------------
  (local-set-many-keys   
   [(f12)]              'switch-frame-next-powershell
   [S-f12]              (ti::definteractive
			 (powershell (generate-new-buffer-name
				      "*PowerShell*")))
   
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
      (shell-eval-region)
    ;; May want to change this to paragraph depending on style of use
    (shell-eval-line-and-step)))

(defun switch-frame-next-powershell ()
  "Switch to next shell buffer." 
  (interactive)
  (switch-frame-next-buffer '("\\*PowerShell") '("^ ")))

(defun switch-frame-previous-powershell ()
  "Switch to previous shell buffer." 
  (interactive)
  (switch-frame-previous-buffer '("\\*PowerShell") '("^ ")))

(defun switch-frame-current-powershell ()
  "Displays the current associated shell buffer."
  (interactive)
  (display-buffer current-shell-buffer))
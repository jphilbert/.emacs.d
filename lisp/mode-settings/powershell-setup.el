;; ----------------------------------------------------------------------------
;; SHELL Mode Setup
;; ----------------------------------------------------------------------------
(provide 'powershell-setup)
(require 'interactive-shell)


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun my-powershell-mode-hook ()
  (auto-fill-mode nil)
  (setq shell-buffer-search-string "*PowerShell")

  ;; -------------------- Key bindings -------------------- 
  (local-set-many-keys
   ;; ---------- Evaluation ----------
   [(shift return)]     'shell-eval

   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "powershell "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "powershell "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-current-powershell
   [S-f12]              (ti::definteractive
                         (powershell (generate-new-buffer-name
                                      "*PowerShell*")))
   [C-f12]              'shell-buffer-choose
   
   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))

(defun my-powershell-hook ()
  (auto-fill-mode nil)
  (text-scale-set -1.1)
  
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
   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "powershell "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "powershell "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-next-powershell
   [S-f12]              (ti::definteractive
			 (powershell (generate-new-buffer-name
				      "*PowerShell*")))

   ;; ---------- Auto Pairing ----------
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
  (switch-frame-next-buffer '("\\*PowerShell") '("^ ") t)
  (end-of-buffer-all))

(defun switch-frame-previous-powershell ()
  "Switch to previous shell buffer." 
  (interactive)
  (switch-frame-previous-buffer '("\\*PowerShell") '("^ ") t)
  (end-of-buffer-all))

(defun switch-frame-current-powershell ()
  "Displays the current associated shell buffer."
  (interactive)
  (display-buffer current-shell-buffer)
  (end-of-buffer))

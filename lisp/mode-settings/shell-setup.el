;; ----------------------------------------------------------------------------
;; SHELL / BATCH Mode Setup
;; ----------------------------------------------------------------------------
(provide 'shell-setup)

(require 'interactive-shell)
(require 'ntcmd) 

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun my-bat-mode-hook ()
  (auto-fill-mode nil)
   )

(defun my-shell-mode-hook ()
  (auto-fill-mode nil)
  (text-scale-set -1.1)

  (setq ansi-color-for-comint-mode              t
        comint-scroll-to-bottom-on-input        t
        comint-scroll-to-bottom-on-output       t
        comint-move-point-for-output            t
        comint-prompt-read-only                 t)
  )

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys ntcmd-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]     'shell-eval

  ;; ---------- Help ----------
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "dos "))
  (kbd "C-h W")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "dos "))

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-shell
  [S-f12]              'shell-new
  [C-f12]              'shell-buffer-choose
  
  ;; ---------- Auto Pairing ----------
  (kbd "(")            'skeleton-pair-insert-maybe
  (kbd "[")            'skeleton-pair-insert-maybe
  (kbd "{")            'skeleton-pair-insert-maybe
  (kbd "\"")           'skeleton-pair-insert-maybe
  (kbd "\'")           'skeleton-pair-insert-maybe
  (kbd "\`")           'skeleton-pair-insert-maybe)  

(define-many-keys shell-mode-map
   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "dos "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "dos "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-next-shell
   [S-f12]              'shell-new

   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe)

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


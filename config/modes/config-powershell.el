;; ----------------------------------------------------------------------------
;; SHELL Mode Setup
;; ----------------------------------------------------------------------------
(provide 'config-powershell)
(require 'powershell)

(setq powershell-indent 2)

(defconst powershell-function-names-regex
  ;; Syntax detected is [scope:]verb-noun
  ;; Match 0 is the entire name.
  ;; Match 1 is the scope if any.
  ;; Match 2 is the function name (which must exist)
  (concat
   "\\_<\\(?:" (regexp-opt powershell-scope-names t) ":\\)?"
   "\\([A-Z][a-zA-Z0-9]*-[A-Z0-9][a-zA-Z0-9-]*\\)\\_>")
  "Identifies legal function & filter names.")

(setq-default powershell-font-lock-keywords-3
  (append
   powershell-font-lock-keywords-2
   `( ;; user variables
     (,powershell-variables-regexp
      (0 font-lock-variable-name-face)
      (1 (cons font-lock-type-face '(underline)) t t)
      (2 (cons font-lock-type-face '(underline)) t t))
     ;; function argument names
     (,powershell-function-switch-names-regexp
      (0 font-lock-constant-face)
      (1 (cons font-lock-type-face '(underline)) t t)
      (2 (cons font-lock-type-face '(underline)) t t))
     ;; function names
     (,powershell-function-names-regex
      (0 font-lock-function-name-face)
      (1 (cons font-lock-type-face '(underline)) t t)))))


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
   ;; No longer using TinyTools
   ;; [S-f12]              (ti::definteractive
   ;;                       (powershell (generate-new-buffer-name
   ;;                                    "*PowerShell*")))
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
   ;; No longer using TinyTools
   ;; [S-f12]              (ti::definteractive
   ;; 			 (powershell (generate-new-buffer-name
   ;; 				      "*PowerShell*")))

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
;; (defun shell-eval ()
;;   "Evaluates Shell commands in a script"
;;   (interactive) 
;;   (if (and transient-mark-mode mark-active)
;;       (shell-eval-region)
;;     ;; May want to change this to paragraph depending on style of use
;;     (shell-eval-line-and-step)))

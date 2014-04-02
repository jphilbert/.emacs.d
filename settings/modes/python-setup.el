;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)
(require 'python)

;; Jedi
(autoload 'jedi:setup "jedi" nil t)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'python-mode-hook		'my-python-mode-hook)
(defun my-python-mode-hook ()
  (interactive)
  (setq jedi:setup-keys		t
	jedi:complete-on-dot	t)
  (jedi:setup)
  (auto-complete)
  
  (lambda-mode 1)
  
  (hs-minor-mode t)
  (auto-indent-minor-mode -1)
  
  ;; (hs-hide-all)				; Breaks if bad code
  (flyspell-prog-mode)
  
  ;; --------------------------------------------------------------------------
  ;; Key Binding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   [(return)]		'newline-and-indent
   
   ;; ---------- Evaluation ----------
   [(shift return)]     'python-eval

   ;; ---------- Indent / Tabs ----------
   (kbd "<C-tab>")	'tab-to-tab-stop-magic
   (kbd "<tab>")        'indent-for-tab-command  

   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "Python "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "Python "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-current-python
   ;; [S-f12]              'python-process-new
   ;; [C-f12]              'python-process-set 
   
   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe  
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))

(add-hook 'inferior-python-mode-hook	'my-inferior-python-mode-hook)
(defun my-inferior-python-mode-hook ()
  (setq jedi:setup-keys		t
	jedi:complete-on-dot	t)
  (jedi:setup)

  (text-scale-set -1.1)

  (lambda-mode 1)

  ;; --------------------------------------------------------------------------
  ;; Key-binding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "Python "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "Python "))

   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-previous
   
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
(defun python-eval ()
  "Evaluates python code based on context."
  (interactive)
  ;; Eval
  (if (and transient-mark-mode mark-active)
      (python-eval-region)
    (if (or (> (current-indentation) 0)
	    (python-info-looking-at-beginning-of-defun))
	(python-eval-defun)
      (python-eval-paragraph)))
  (switch-frame-current-python)
  (switch-frame-previous))

(defun python-eval-paragraph ()
  "Evaluates python region"
  (interactive)
  (save-excursion
    (progn (mark-paragraph)
	   (call-interactively 'python-shell-send-region)))
  (forward-paragraph))

(defun python-eval-region ()
  "Evaluates python region"
  (interactive)
  (let ((end-mark (region-end)))
    (call-interactively 'python-shell-send-region)
    (goto-char end-mark)))

(defun python-eval-defun ()
  "Evaluates python function."
  (interactive)
  (call-interactively 'python-shell-send-defun)
  (end-of-defun))


(defun python-process-new ()
  "Creates a new python-process."
  (interactive)
  (call-interactively 'run-python)
  (python-shell-switch-to-shell)
  (switch-frame-previous))

(defun switch-frame-current-python ()
  "Switch to current python process buffer."
  (interactive)
  (python-shell-switch-to-shell)
  (end-of-buffer-all))


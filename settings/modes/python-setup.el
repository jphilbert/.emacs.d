;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)
(require 'python-mode)

;; Jedi
(autoload 'jedi:setup "jedi" nil t)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'python-mode-hook		'my-python-mode-hook)
(defun my-python-mode-hook ()
  (setq jedi:setup-keys		t
	jedi:complete-on-dot	t)
  (jedi:setup)

  (setq py-shell-switch-buffers-on-execute nil)

  (lambda-mode 1)
  
  (hs-minor-mode t)
  (auto-indent-minor-mode 1)
  
  (hs-hide-all)
  (flyspell-prog-mode)
  
  ;; --------------------------------------------------------------------------
  ;; Key Binding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   ;; ---------- Evaluation ----------
   [(shift return)]     'python-eval

   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "Python "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "Python "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-current-python
   [S-f12]              'python-process-new
   
   ;; ---------- Auto Pairing ----------
   ;; (kbd "(")            'skeleton-pair-insert-maybe ; Broke 
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe))

(add-hook 'py-shell-hook		'my-inferior-python-mode-hook)
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
   
   ;; ---------- Auto Pairing ----------
   ;; (kbd "(")            'skeleton-pair-insert-maybe ; Broke 
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
    (python-eval-block-or-def)))

(defun python-eval-region ()
  "Evaluates python region"
  (interactive)
  (call-interactively 'py-execute-region))

(defun python-eval-block-or-def ()
  "Evaluates python def/class or block."
  (interactive)
  (condition-case nil
      (py-execute-def-or-class)
    (error
     (py-mark-block)
     (call-interactively 'py-execute-region))))

(defun python-process-new ()
  "Creates a new python-process."
  (interactive)
  (py-shell)
  (switch-frame-previous))

(defun switch-frame-next-python ()
  "Cycle through the python buffers."
  (interactive)
  (switch-frame-next-buffer '("\\*python.*") '("^ ")))

(defun switch-frame-current-python ()
  "Switch to current python process buffer and move cursor to the end."
  (interactive)
  (if (eq (length (switch-frame-buffer-list '("\\*Python.*") '("^ "))) 0)
      (python-process-new)
    (py-shell))
  (goto-char (point-max)))

(defun R-kill-all-processes ()
  "Kills all R processes and clears the name-list."
  (interactive)
  (mapcar '(lambda (arg)
             (when (get-process (car arg))
               (kill-process (get-process (car arg)))))
          ess-process-name-list)
  
  (mapcar 'kill-buffer (switch-frame-buffer-list '("\\*R.*") '("^ ")))
  (setq ess-process-name-list nil))

;; Expand-Region is still calling PY-GOTO-BEYOND-CLAUSE. This fixes it.
(defalias 'py-goto-beyond-clause 'py-end-of-clause)
;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)
(require 'python)

;; Jedi
(autoload 'jedi:setup "jedi" nil t)

(setq py-split-windows-on-execute-p nil)

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
  (auto-indent-minor-mode -1)
  
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
   (kbd "(")            'skeleton-pair-insert-maybe ; Broke 
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

   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-previous
   
   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe ; Broke 
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
    (python-eval-block-or-def))
  (save-frame-excursion
   (raise-frame (get-frame "*Python*"))))

(defun python-eval-region ()
  "Evaluates python region"
  (interactive)
  (call-interactively 'py-execute-region))

(defun python-eval-block-or-def ()
  "Evaluates python def/class or block."
  (interactive)
  (let ((p nil))
    (condition-case nil
	(py-execute-def-or-class)
      (error
       (progn
	 (save-excursion
	   (py-mark-block)
	   (setq p (- (region-beginning) (region-end)))
	   )
	 (if (eq p 0)
	     (py-execute-line)
	   (progn
	    (py-execute-block)
	    (py-end-of-block)))
	   (next-non-blank-line))
       ))))

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

(defun switch-frame-current-python ()
  "Switch to current python process buffer."
  (interactive)
  (let (b)
    (if (>= (length (switch-frame-buffer-list '("\\*Python.*") '("^ "))) 1)
	(progn
	  ;; (setq b (process-buffer (get-process
	  ;; 			   (py-process-name))))
	  (raise-frame (get-frame "*Python*"))
	  ;; (set-buffer b)
	  (end-of-buffer-all))
      (message "no python process"))))

;; Expand-Region is still calling PY-GOTO-BEYOND-CLAUSE. This fixes it.
(defalias 'py-goto-beyond-clause 'py-end-of-clause)


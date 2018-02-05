;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)
;; (require 'python)

;; requires ac-anaconda
;; requires anaconda-mode

(require 'anaconda-mode)

(custom-set-variables
 '(python-guess-indent nil)
 '(python-indent 4))

;; Set if not in path
(setq python-shell-interpreter "C:/ProgramData/Anaconda3/python.exe")

;; fixes odd random error in emacs
;; (add-to-list 'process-coding-system-alist
;;		   '("python" cp1251-unix . cp1251-unix))

;; (setq-default python-shell-prompt-detect-failure-warning nil)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'python-mode-hook		'my-python-mode-hook)
(defun my-python-mode-hook ()
  (interactive)
  (anaconda-mode)
  (ac-anaconda-setup)
  ;; (auto-complete)
  
  (hs-minor-mode t)
  ;; (auto-indent-minor-mode -1)
  
  ;; (hs-hide-all)				; Breaks if bad code
  (flyspell-prog-mode)
  (turn-on-auto-fill)
  )

(add-hook 'inferior-python-mode-hook	'my-inferior-python-mode-hook)
(defun my-inferior-python-mode-hook ()
  (ac-anaconda-setup) 
  (python-shell-switch-to-shell)
  (text-scale-set -1.1)
  )


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun python-eval ()
  "Evaluates python code based on context."
  (interactive)

  ;; Start a shell if needed
  (unless (python-shell-get-process)
    (run-python (python-shell-parse-command) nil))
  
  ;; Eval
  (if (and transient-mark-mode mark-active)
      (python-eval-region)
    (if (or (> (current-indentation) 0)
	    (python-info-looking-at-beginning-of-defun))
	(python-eval-defun)
	(python-eval-paragraph)))
  (python-nav-forward-statement)
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

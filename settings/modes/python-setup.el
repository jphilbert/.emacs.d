;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)
(require 'python)

;; requires ac-anaconda
;; requires anaconda-mode

;; (require 'anaconda-mode)

(custom-set-variables
 '(python-guess-indent nil)
 '(python-indent 4))

(setq jedi:setup-keys		t
	 jedi:complete-on-dot	t)

;; Set if not in path
;; Best to set the PATH
;; Python --> /Anaconda3/python.exe
;; Conda, PIP, etc. --> /Anaconda3/scripts

;; (setq python-shell-interpreter		; This does not have packages
;; 	 "C:/ProgramData/Anaconda3/python.exe")

(setq python-shell-interpreter		; Use this one
	 "C:/Users/hilbertjp2/AppData/Local/Continuum/anaconda3/python.exe")


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
  ;; (anaconda-mode)
  ;; (ac-anaconda-setup)
  ;; (auto-complete)

  (jedi:setup)
  (auto-complete)
  
  (hs-minor-mode t)
  ;; (auto-indent-minor-mode -1)
  
  ;; (hs-hide-all)				; Breaks if bad code
  (flyspell-prog-mode)
  (turn-on-auto-fill)
  )

(add-hook 'inferior-python-mode-hook	'my-inferior-python-mode-hook)
(defun my-inferior-python-mode-hook ()
  (jedi:setup)
  (auto-complete)
  
  (text-scale-set -1.1)
  )

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys python-mode-map
  [(return)]		'newline-and-indent
  (kbd "<C-next>")		'python-nav-forward-defun
  (kbd "<C-prior>")		'python-nav-backward-defun
  
  ;; ---------- Evaluation ----------
  [(shift return)]     'python-eval

  ;; ---------- Indent / Tabs ----------
  (kbd "<S-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'indent-for-tab-command  

  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "Python "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h f")   	'jedi:show-doc
  "\C-hv"      	'python-object-info
  
  ;; ---------- Frame Switching ----------
  [(f12)]              'python-shell-switch-to-shell
  ;; [S-f12]              'python-process-new
  ;; [C-f12]              'python-process-set 
  )

(define-many-keys inferior-python-mode-map
  [S-C-up]		'previous-line
  [S-C-down]		'next-line
  
  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "Python "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h f")   	'anaconda-mode-show-doc

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-previous
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

;; -----------------------------------------------------------------------------
;;  Help Functions
;; -----------------------------------------------------------------------------
(defun python-object-info ()
  "Get info for object at point"
  (interactive)
  (let ((objname (current-word)))
    (python-shell-send-string (concat "type(" objname ")"))
    (python-shell-send-string (concat "[i for i in dir(" objname
							   ") if i[0] != '_']"))))

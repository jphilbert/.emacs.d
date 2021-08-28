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

;; (setq python-shell-interpreter		; python.exe does not support pyplot
;; 	 "C:/Users/hilbertjp2/AppData/Local/Continuum/anaconda3/python.exe")

(setq-default python-shell-interpreter	; USE THIS
		    "C:/Users/hilbertjp2/AppData/Local/Continuum/anaconda3/Scripts/ipython.exe")


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

  ;; (jedi:setup)
  (auto-complete)
  
  (hs-minor-mode t)
  ;; (auto-indent-minor-mode -1)
  
  ;; (hs-hide-all)				; Breaks if bad code
  (flyspell-prog-mode)
  (turn-on-auto-fill)
  )

(add-hook 'inferior-python-mode-hook	'my-inferior-python-mode-hook)
(defun my-inferior-python-mode-hook ()
  ;; (jedi:setup)
  (auto-complete)
  
  (text-scale-set -1.1)
  )

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys python-mode-map
  [(return)]			'newline-and-indent
  (kbd "<C-next>")		'python-nav-forward-defun
  (kbd "<C-prior>")		'python-nav-backward-defun
  
  ;; ---------- Evaluation ----------
  [(shift return)]		'python-eval
  [(M return)]			'python-eval-echo
  ;; [(M return)]			'python-print-last

  ;; ---------- Indent / Tabs ----------
  (kbd "<S-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'indent-for-tab-command
  (kbd "C-<")			'python-indent-shift-left
  (kbd "C->")			'python-indent-shift-right

  ;; ---------- Help ----------
  [(f1)]		   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "Python "))
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h f")   	'python-object-help
  "\C-hv"      	'python-object-info
  
  ;; ---------- Frame Switching ----------
  [(f12)]              'python-shell-switch-to-shell
  [S-f12]              'python-process-new
  [C-f12]              'python-process-set 
  )

(define-many-keys inferior-python-mode-map
  [S-C-up]		'previous-line
  [S-C-down]		'next-line
  
  ;; ---------- Help ----------
  [(f1)]		   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "Python "))
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "Python "))
  (kbd "C-h f")   	'python-object-help
  "\C-hv"      	'python-object-info

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
    (goto-char end-mark)
    (deactivate-mark)))

(defun python-eval-defun ()
  "Evaluates python function."
  (interactive)
  (call-interactively 'python-shell-send-defun)
  (end-of-defun))

(make-variable-buffer-local 'python-shell-buffer-name)
(defun python-process-new ()
  "Creates a new python-process."
  (interactive)
  
  ;; Get and Set new Python Shell Name
  (let ((v 0))
    ;; Count all the Python Shells
    (dolist (elt (buffer-list) v)
	 (let* ((buf (buffer-name elt))
		   (pos (string-match "^\\*Python\[ \]*\\(\[0-9\]*\\)\\*" buf)))
	   (when pos
		(message buf)	   
		(setq v (max 1 v (string-to-int (match-string 1 buf))))
		)))
    ;; If there is one, increment
    (if (> v 0)
	   (setq v (concat " " (number-to-string (1+ v))))
	 (setq v ""))
    ;; Set this buffer's python-shell
    (setq python-shell-buffer-name (concat "Python" v)))

  ;; Run
  (run-python (python-shell-parse-command))
  (python-shell-switch-to-shell)		; pop the frame
  (switch-frame-previous)			; switch back
  )

;; Switch / Set Python Shell
(defun python-process-set ()
  (interactive)
  ;; Get the list of python-shell buffers
  (let ((icicle-buffer-complete-fn (list)))
    (dolist ($buf (buffer-list (current-buffer)))
	 (with-current-buffer $buf
	   (when (eq major-mode 'inferior-python-mode)
		(add-to-list 'icicle-buffer-complete-fn
				   (buffer-name $buf)))))

    ;; ask, delete the *, and set
    (setq
	python-shell-buffer-name
	(replace-regexp-in-string
	 "\*" ""
	 (read-buffer
	  "New Python Buffer: "
	  (concat "*" python-shell-buffer-name "*")
	  t)))))

(defun switch-frame-current-python ()
  "Switch to current python process buffer."
  (interactive)
  (python-shell-switch-to-shell)
  (end-of-buffer-all))

(defun python-print-last ()
  "Prints last result"
  (interactive)
  (python-shell-send-string "print(_)"))

(defun python-shell-send-region-echo (start end &optional send-main)
  "Same as PYTHON-SHELL-SEND-REGION but echos the output. Does not work if the region has multiple commands."
  (interactive "r\nP")
  (let* ((string (python-shell-buffer-substring start end (not send-main)))
         (process (python-shell-get-or-create-process))
	    (buffer (current-buffer))
         (original-string (string-trim
					  (buffer-substring-no-properties start end)))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (set-buffer (process-buffer process))
    (let ((oldpoint (point)))
      (goto-char (process-mark process))
      (insert (concat original-string "\n"))
      (set-marker (process-mark process) (point))
      (goto-char oldpoint))
    (set-buffer buffer)
    ;; (message "Sent: %s..." (match-string 1 original-string))
    (python-shell-send-string original-string
    						process)))

(defun python-eval-echo ()
  "Evaluates python code based on context."
  (interactive)

  ;; Start a shell if needed
  (unless (python-shell-get-process)
    (run-python (python-shell-parse-command) nil))
  
  ;; Eval
  (if (and transient-mark-mode mark-active)
      (let ((end-mark (region-end)))
	   (call-interactively 'python-shell-send-region-echo)
	   (goto-char end-mark)
	   (deactivate-mark))
    (save-excursion
	 (progn (mark-paragraph)
		   (call-interactively 'python-shell-send-region-echo)))
    (forward-paragraph))
  (python-nav-forward-statement)
  (switch-frame-current-python)
  (switch-frame-previous))



;; -----------------------------------------------------------------------------
;;  Help Functions
;; -----------------------------------------------------------------------------
(defun python-object-help()
  "Get Help for object at point"
  (interactive)
  ;; Mark the object
  (let (reg-begin reg-end objname)
    (if (use-region-p)
	   (setq reg-begin (region-beginning)
		    reg-end (region-end))
	 (setq reg-begin (- (save-excursion
					  (re-search-forward "[^a-zA-Z_.]" nil t)) 1)
		  reg-end (+ (save-excursion
					  (re-search-backward "[^a-zA-Z_.]" nil t)) 1)))
    (setq objname (buffer-substring reg-begin reg-end))
    (with-help-window "*Help*"
	 ;; (princ objname)
	 ;; (princ "\n")
	 ;; (princ (make-string (length objname) ?-))
	 ;; (princ "\n")
	 (princ (python-shell-send-string-no-output
		    (concat "help(" objname ")")))
	 )))

(defun python-object-info ()
  "Get Type & Methods for object at point"
  (interactive)
  ;; Mark the object
  (let (reg-begin reg-end objname)
    (if (use-region-p)
	   (setq reg-begin (region-beginning)
		    reg-end (region-end))
	 (setq reg-begin (- (save-excursion
					  (re-search-forward "[^a-zA-Z_.]" nil t)) 1)
		  reg-end (+ (save-excursion
					  (re-search-backward "[^a-zA-Z_.]" nil t)) 1)))
    (setq objname (buffer-substring reg-begin reg-end))
    (with-help-window "*Help*"
	 (princ objname)
	 (princ "\n-- TYPE --\n")
	 (princ (python-shell-send-string-no-output
		    (concat "type(" objname ")")))
	 (princ "\n-- METHODS --\n")
	 (princ (python-shell-send-string-no-output
		    (concat "print('\\n'.join([i for i in dir(" objname
				  ") if i[0] != '_']))"))))))



;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting
;; ------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'python-mode        
 '(("[\\<-][0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("[!<=>]=\\|[<>]\\|\\(\\b\\(and\\|or\\|not\\|in\\|is\\)\\b\\)"
     .
     'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'inferior-python-mode
 '(("[\\<-][0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)))


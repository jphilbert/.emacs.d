;; ----------------------------------------------------------------------------
;; Python Mode Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun my-python-mode-hook ()
  (interactive)
  
  (hs-minor-mode t)
  (setq ac-sources
	   (append '(ac-source-yasnippet) ac-sources))
  (flyspell-prog-mode)
  (turn-on-auto-fill)  
  (rainbow-delimiters-mode 1))

(defun my-inferior-python-mode-hook () 
  (text-scale-set -1.1))

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys python-mode-map
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
  [(f1)]				'(lambda ()
					   (interactive)
					   (google-query-at-point t "Python "))
  [(S-f1)]			'(lambda ()
					   (interactive)
					   (google-query-at-point nil "Python "))
  (kbd "C-h w")		'(lambda ()
					   (interactive)
					   (google-query-at-point nil "Python "))
  (kbd "C-h f")		'python-object-help
  "\C-hv"				'python-object-info
  
  ;; ---------- Frame Switching ----------
  [(f12)]				'python-switch-frame-process
  [S-f12]				'python-process-new
  [C-f12]				'python-process-set 
  )

(define-keys inferior-python-mode-map
  [S-C-up]			'previous-line
  [S-C-down]			'next-line
  
  ;; ---------- Help ----------
  [(f1)]				'(lambda ()
					   (interactive)
					   (google-query-at-point t "Python "))
  [(S-f1)]			'(lambda ()
					   (interactive)
					   (google-query-at-point nil "Python "))
  (kbd "C-h w")		'(lambda ()
					   (interactive)
					   (google-query-at-point nil "Python "))
  (kbd "C-h f")		'python-object-help
  "\C-hv"				'python-object-info

  ;; ---------- Frame Switching ----------
  [(f12)]				'python-switch-frame-script
  )

;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------

;; ---------- Evaluation ---------- ;;
(defun python-eval ()
  "Evaluates python code based on context."
  (interactive)

  ;; Start a shell if needed
  (unless (python-shell-get-process)
    (run-python (python-shell-parse-command) nil))
  
  ;; Eval
  (cond
   ((and transient-mark-mode mark-active)
    (python-eval-region))
   ((or (> (current-indentation) 0)
	   (python-info-looking-at-beginning-of-defun))
    (python-eval-defun))
   (t
    (python-eval-paragraph)))
  (python-nav-forward-statement)
  (python-raise-frame-process))

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
  "Evaluates python function"
  (interactive)
  (call-interactively 'python-shell-send-defun)
  (end-of-defun))

;; ---------- Process Commands ---------- ;;
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
  (python-raise-frame-process)		; raise but not select
  )

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


;; ---------- Frame Commands ---------- ;;
(defun python-switch-frame-process ()
  "Switch to associated process, associate with one, or create one."
  (interactive)
  ;; Does current buffer have an associated process?
  ;; Yes -> raise and select
  ;; No -> are there processes running?
  ;; Yes -> associate -> raise
  ;; No -> create one -> associate -> raise 
  (python-shell-switch-to-shell)
  (end-of-buffer-all))

(defun python-raise-frame-process ()
  (save-frame-excursion
   (raise-frame
    (get-frame (concat "*" python-shell-buffer-name "*")))))

(defun python-switch-frame-script ()
  "Switch to most recent script buffer."
  (interactive)
  (let ((loc-proc-name python-shell-buffer-name)
	   (blist (cdr (buffer-list))))
    (while (and blist
			 (with-current-buffer (car blist)
			   (not (and
				    (equal major-mode 'python-mode)
				    (equal loc-proc-name python-shell-buffer-name)))))
	 (pop blist))
    (if blist
	   (display-buffer (car blist) t)
	 (message "Found no python buffers for process %s"
			loc-proc-name))))


;; ---------- Echo Results ---------- ;;
(defun python-print-last ()
  "Prints last result"
  (interactive)
  (python-shell-send-string "print(_)"))

(defun python-shell-send-region-echo (start end &optional send-main)
  "Same as PYTHON-SHELL-SEND-REGION but echos the output in the process buffer. Does not work if the region has multiple commands."
  (interactive "r\nP")
  (let* ((string
		(python-shell-buffer-substring start end (not send-main)))
         (process
		(python-shell-get-or-create-process))
	    (buffer
		(current-buffer))
         (original-string
		(string-trim (buffer-substring-no-properties start end)))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    
    (set-buffer (process-buffer process))
    (let ((oldpoint (point)))
      (goto-char (process-mark process))
      (insert (concat original-string "\n"))
      (set-marker (process-mark process) (point))
      (goto-char oldpoint))
    
    (set-buffer buffer)
    (python-shell-send-string original-string
    						process)))

(defun python-eval-echo ()
  "Evaluates python code based on context and echo."
  (interactive)
  (cl-letf (((symbol-function 'python-shell-send-region)
		   #'python-shell-send-region-echo))
    (call-interactively 'python-eval))) 


;; ---------- Help Commands ---------- ;;
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
 '(
   ;; Numbers
   ("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b"
    .
    'font-lock-number-face)
   
   ;; Relational Operators
   ;;  - These we don't require a space
   ("\\(\\sw\\|\\s\"\\|\\s-\\)\\([<=>]=\\|[!<>]\\)\\(\\sw\\|\\s\"\\|\\s-\\)"
    2
    'font-lock-relation-operator-face)
   ;;  - These must have a white space before and after
   ("\\s-\\([<=>]=\\|[!<>]\\|and\\|or\\|not\\|in\\|is\\)\\s-"
    .
    'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'inferior-python-mode
 '(("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b"
    .
    'font-lock-number-face)))


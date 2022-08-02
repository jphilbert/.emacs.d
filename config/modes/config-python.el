
;;; config-python.el --- Emacs Config: python.el configuration.

;;; Code:
(require 'config-programming)
(require 'python)

(setq
 python-guess-indent                nil
 python-indent                      4
 python-indent-guess-indent-offset  nil
 python-indent-offset               4
 python-shell-interpreter           (or
                                     (config-get :applications :python)
                                     python-shell-interpreter))


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-python ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;; (eldoc-mode +1)
  ;; (setq-local electric-layout-rules
  ;;             '((?: . (lambda ()
  ;;                       (and (zerop (first (syntax-ppss)))
  ;;                            (python-info-statement-starts-block-p)
  ;;                            'after)))))
  ;; (when (fboundp #'python-imenu-create-flat-index)
  ;;   (setq-local imenu-create-index-function
  ;;               #'python-imenu-create-flat-index))
  ;; (add-hook 'post-self-insert-hook
  ;;           #'electric-layout-post-self-insert-function nil 'local)
  ;; (when config-python-mode-set-encoding-automatically
  ;;   (add-hook 'after-save-hook 'config-python-mode-set-encoding nil 'local))
)

(defun config-mode-python-interactive ()
  "Defaults for Python REPL buffer."
  )

(add-hook 'python-mode              'config-mode-python)
(add-hook 'inferior-python-mode     'config-mode-python-interactive)


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
;; Commands
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


;;; Encoding detection/insertion logic
;;
;; Adapted from ruby-mode.el
;; This logic was useful in Python 2, but it's not really needed in Python 3.
(defun python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (config-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (config-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at
                  "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (config-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))




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


(provide 'config-python)
;;; CONFIG-PYTHON.EL ends here



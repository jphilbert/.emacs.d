
;;; config-python.el --- Emacs Config: python.el configuration.

;;; Code:
(require 'config-programming)
(require 'repl)
(require 'python)

(setq
 python-guess-indent                nil
 python-indent                      4
 python-indent-guess-indent-offset  nil
 python-indent-offset               4)

(setq-from-config python-shell-interpreter      :applications :python :exe)
(setq-from-config python-shell-interpreter-args :applications :python :args)


(setq smart-hungry-delete-major-mode-dedent-function-alist
      (--remove (eq 'python-mode (car it))
                smart-hungry-delete-major-mode-dedent-function-alist))
(add-to-list
 'smart-hungry-delete-major-mode-dedent-function-alist
 '(python-mode
   .
   (lambda ()
     (interactive)
     (python-indent-dedent-line-backspace 1))))


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-python ()
  "Defaults for Python programming."
  (subword-mode +1)
  (eldoc-mode +1)
  (repl-mode +1)
  (setq
   repl-interactive-mode        'inferior-emacs-lisp-mode
   repl-function-eval           #'python-eval
   ;; repl-function-eval-insert    #'python-eval-insert
   repl-function-set            #'python-set-repl
   repl-function-create         #'python-create-repl)


  ;; (setq-local electric-layout-rules
  ;;             '((?: . (lambda ()
  ;;                       (and (zerop (first (syntax-ppss)))
  ;;                            (python-info-statement-starts-block-p)
  ;;                            'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  ;; (add-hook 'post-self-insert-hook
  ;;           #'electric-layout-post-self-insert-function nil 'local)
  ;; (when config-python-mode-set-encoding-automatically
  ;;   (add-hook 'after-save-hook 'config-python-mode-set-encoding nil 'local))
)

(defun config-mode-python-interactive ()
  "Defaults for Python REPL buffer."
  (repl-mode +1)
  )

(add-hook 'python-mode-hook              'config-mode-python)
(add-hook 'inferior-python-mode-hook     'config-mode-python-interactive)


;; --------------------------------------------------------------------------
;; Frame Settings
;; --------------------------------------------------------------------------
(add-to-list
 'display-buffer-alist
 '("\\*Python.*\\*"
   (display-buffer-reuse-window display-buffer-pop-up-frame)
   (cascade .                   nil)
   (font-size .                 100)

   (pop-up-frame-parameters
    .
    ((top .                     20)
	 (left .                    810) 
	 (height .                  0.6) 
     (unsplittable .            t)
	 ))))


;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys python-mode-map
  (kbd "<C-next>")		'python-nav-next-defun
  (kbd "<C-prior>")		'python-nav-back-defun
  
  ;; ---------- Evaluation ----------
  ;; [(shift return)]		'python-eval
  ;; [(M return)]			'python-eval-echo
  ;; [(M return)]			'python-print-last

  ;; ---------- Indent / Tabs ----------
  (kbd "<S-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")         'indent-for-tab-command
  (kbd "C-<")			'python-indent-shift-left
  (kbd "C->")			'python-indent-shift-right

  ;; ---------- Help ----------
  (kbd "C-h f")         'python-object-help
  "\C-hv"				'python-object-info
  )

(define-keys inferior-python-mode-map
  ;; [S-C-up]			'previous-line
  ;; [S-C-down]			'next-line
  
  ;; ---------- Help ----------
  (kbd "C-h f")         'python-object-help
  "\C-hv"				'python-object-info
  )

;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------
(defun python-nav-back-defun ()
  "Move point to last DEF.

Same as `python-nav-backward-defun' but also moves to beginning
of statement (see `python-nav-beginning-of-statement')."
  (interactive)
  (python-nav-backward-defun)
  (python-nav-beginning-of-statement))

(defun python-nav-next-defun ()
  "Move point to next DEF.

Similar to `python-nav-forward-defun' but also moves to beginning
of statement (see `python-nav-beginning-of-statement') as oppose
to end of definition. Unlike `python-nav-forward-defun', this
moves to the next regardless where a point is on a current
definition line."
  (interactive)
  (when (python-info-looking-at-beginning-of-defun)
    (end-of-line))
  (python-nav-forward-defun)
  (python-nav-beginning-of-statement))

(defun python-info-defun-bounds (&optional ignore-decorators)
  "Finds the start and end point of current Python definition.

Return a list with the start and end points of the root defun
POINT is currently in, otherwise nil when the point is not in a
definition. Argument IGNORE-DECORATORS is non-nil do not include
decorators. Code extracted from `python-shell-send-defun'."
  (save-excursion
    (when-let
        (;; Save the current point
         (pt_cur (point))
         ;; Move back to a top-level definition
         ;;   Since python-nav-beginning-of-defun will move point even if
         ;;   not within a defun, this will exit if so.
         ((progn
            (end-of-line 1)
            (while (and (or (python-nav-beginning-of-defun)
                            (beginning-of-line 1))
                        (> (current-indentation) 0)))
            (python-info-looking-at-beginning-of-defun)))

         ;; Return the point after checking for decorators
         (pt0
          (progn
            (unless ignore-decorators
              (while (and
                      (eq (forward-line -1) 0)
                      (if (looking-at (python-rx decorator))
                          t
                        (forward-line 1)
                        nil))))
            (point-marker)))

         ;; Go to the end of the defun and return
         (pt1
          (progn
            (or (python-nav-end-of-defun)
                (end-of-line 1))
            (point-marker)))

         ;; Check to make sure initial point was before the end
         ((< pt_cur pt1)))

      ;; Success
      (list pt0 pt1))))

(defun python-info-block-bounds ()
  "Finds the start and end point of current Python block.

Return a list with the start and end points of the block POINT is
currently in, otherwise nil when the point is not in a
definition."
  (save-excursion
    (when-let
        (;; Move back to start of block
         ;;   Exit if not a block start
         ((progn
            (python-nav-beginning-of-block)
            (python-info-beginning-of-block-p)))

         (pt0 (point-marker))

         (pt1
          (progn
            (python-nav-end-of-block)
            (point-marker))))

      ;; Success
      (list pt0 pt1))))

;; ---------- Process Commands ---------- ;;
(make-variable-buffer-local 'python-shell-buffer-name)

(defun python-create-repl ()
  "Creates a new Python process and frame."
  (interactive) 
  (setq python-shell-buffer-name
        (s-with
            (default-value 'python-shell-buffer-name)
          (format "*%s [%%s]*")
          (generate-new-buffer-name+)
          (s-replace "*" "")))
  
  (let ((calling-frame
         (frame-get))
        (python-buffer
         (python-shell-make-comint
          (python-shell-calculate-command)
          python-shell-buffer-name
          t)))
    (raise-frame calling-frame)
    python-buffer))

(buffer-name (car (--filter
 (with-current-buffer it
   (eq major-mode 'inferior-python-mode))
 (buffer-list))))

(defun python-set-repl ()
  (interactive)
  (when-let*
      ((buffers
         (--filter
          (with-current-buffer it
            (eq major-mode 'inferior-python-mode))
          (buffer-list)))
       (new-buffer
        (cond
         ((> (length buffers) 1)
          (read-buffer "Python REPL buffer to use: "
                       nil t
                       (lambda (it)
                         (with-current-buffer (car it)
                           (eq major-mode 'inferior-python-mode)))))
         ((= (length buffers) 1)
          (car buffers)))))
    (setq python-shell-buffer-name (s-replace "*" "" (buffer-name new-buffer)))
    new-buffer))

;; ---------- Evaluation ---------- ;;
(defun python-eval ()
  "Evaluates python code based on context."
  (interactive)
  (cond
   ((python-eval-region))
   ((python-eval-defun))
   ((python-eval-block))
   (t
    (python-shell-send-statement t)
    (python-nav-forward-statement))))

(defun python-eval-region ()
  "Evaluates a region of Python code.

Same as `python-shell-send-region' except SEND-MAIN is non-nil
and moves mark to the end after deactivating region. This does
nothing if `use-region-p' is nil."
  (interactive)
  (when-let
      (((use-region-p))
       (end-mark (region-end)))
    (python-shell-send-region (region-beginning) (region-end) t)
    (goto-char end-mark)
    (deactivate-mark)))
  
(defun python-eval-defun ()
  "Evaluates current Python defun.

Similar to `python-shell-send-defun' except SEND-MAIN is non-nil
and moves point to the next statement. This does nothing if point
is not within a defun."
  (interactive)
  (-when-let*
      (((pt0 pt1) (python-info-defun-bounds)))
    (python-shell-send-region pt0 pt1 t)
    (goto-char pt1)
    (python-nav-forward-statement)
    t))

(defun python-eval-block ()
  "Evaluates current Python block."
  (interactive)
  (-when-let*
      (((pt0 pt1) (python-info-block-bounds)))
    (python-shell-send-region pt0 pt1 t)
    (goto-char pt1)
    (python-nav-forward-statement)
    t))

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
   ("[-+]?\\_<[0-9]+\\(?:\\.?[0-9]+\\)*\\(?:[eE][-+]?[0-9]+\\)?\\_>"
    .
    'font-lock-number-face)
   
   ;; Relational Operators
   ;;  - These we don't require a space
   ("\\([<=>]=\\|[!<>]\\)"
    .
    'font-lock-relation-operator-face)
   ;;  - These must have a white space before and after
   ("\\s-\\([<=>]=\\|[!<>]\\|and\\|or\\|not\\|in\\|is\\)\\s-"
    .
    'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'inferior-python-mode
 '(("[-+]?\\_<[0-9]+\\(?:\\.?[0-9]+\\)*\\(?:[eE][-+]?[0-9]+\\)?\\_>"
    .
    'font-lock-number-face)))


(provide 'config-python)
;;; CONFIG-PYTHON.EL ends here

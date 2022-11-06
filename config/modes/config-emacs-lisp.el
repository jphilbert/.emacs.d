;;; CONFIG-EMACS-LISP.EL --- config for Elisp programming.

;; This file is not part of GNU Emacs.

;;; Code:
(require 'config-programming)
(require 'repl)


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-emacs-lisp ()
  "Sensible defaults for `emacs-lisp-mode'."
  (hs-hide-all)  
  (eldoc-mode +1)
  (dash-fontify-mode +1)
  (repl-mode +1)

  (setq-local
   lisp-indent-function         #'keyword-fix-lisp-indent-function)
  
  (setq
   mode-name                    "eLISP"
   repl-interactive-mode        'inferior-emacs-lisp-mode
   repl-function-eval           #'elisp-eval
   repl-function-eval-insert    #'elisp-eval-insert
   repl-function-set            #'elisp-set-repl
   repl-function-create         #'elisp-setup-repl-frame)


  ;; Recompile your elc when saving an elisp file.
  ;; (add-hook
  ;;  'after-save-hook
  ;;  (lambda ()
  ;;    (when (and
  ;;           (string-prefix-p
  ;;   	     config-root
  ;;   	     (file-truename buffer-file-name))
  ;;           (file-exists-p
  ;;   	     (byte-compile-dest-file buffer-file-name)))
  ;;      (emacs-lisp-byte-compile)))
  ;;  nil
  ;;  t)
  )

(add-hook 'emacs-lisp-mode-hook     'config-mode-emacs-lisp)

(defun config-mode-emacs-lisp-interactive ()
  "Sensible defaults for `ielm'. This mode does not inherit `prog-mode'."
  
  ;; Smart Parenthesis
  (smartparens-mode +1)
  (smartparens-global-strict-mode -1)

  ;; Color Delimiters
  (rainbow-delimiters-mode +1)
  
  (eldoc-mode +1)
  (dash-fontify-mode +1)

  (repl-mode +1))

(add-hook 'ielm-mode-hook           'config-mode-emacs-lisp-interactive)


;; --------------------------------------------------------------------------
;; Frame Settings
;; --------------------------------------------------------------------------
(add-to-list
 'display-buffer-alist
 '("\\*ielm.*\\*"
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

(add-to-list
 'display-buffer-alist
 '("\\*Messages.*\\*"
   (display-buffer-reuse-window display-buffer-pop-up-frame)
   (cascade .                   nil)
   (end-of-buffer-on-display .  t)
   ))

(defun frame-display-ielm-messages (buffer alist)
  (display-buffer-pop-up-frame buffer alist)
  ;; split-window-below
  ;; switch-buffer 
  )


;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; Smart Parenthesis wrapping keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (config-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-[") (config-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (config-wrap-with "\""))

(define-keys		emacs-lisp-mode-map
  ;; ---------- Indent / Tabs ----------
  (kbd "<tab>")     'indent-for-tab-command   
  
  ;; ---------- Help ----------
  "\C-hf"			'elisp-help-function
  "\C-hv"			'elisp-help-variable)

(with-eval-after-load "ielm"
  (define-key ielm-map (kbd "M-(") (config-wrap-with "("))
  (define-key ielm-map (kbd "M-\"") (config-wrap-with "\"")))

(defun keyword-fix-lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation.

https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94"
  
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
	 ;; car of form doesn't seem to be a symbol, or is a keyword
	 ((and (elt state 2)
		   (or (not (looking-at "\\sw\\|\\s_"))
			   (looking-at ":")))
	  (if (not (> (save-excursion (forward-line 1) (point))
			      calculate-lisp-indent-last-sexp))
		  (progn (goto-char calculate-lisp-indent-last-sexp)
			     (beginning-of-line)
			     (parse-partial-sexp (point)
							         calculate-lisp-indent-last-sexp 0 t)))
	  ;; Indent under the list or under the first sexp on the same
	  ;; line as calculate-lisp-indent-last-sexp.  Note that first
	  ;; thing on that line has to be complete sexp since we are
	  ;; inside the innermost containing sexp.
	  (backward-prefix-chars)
	  (current-column))
	 ((and (save-excursion
		     (goto-char indent-point)
		     (skip-syntax-forward " ")
		     (not (looking-at ":")))
		   (save-excursion
		     (goto-char orig-point)
		     (looking-at ":")))
	  (save-excursion
	    (goto-char (+ 2 (elt state 1)))
	    (current-column)))
	 (t
	  (let ((function (buffer-substring (point)
								        (progn (forward-sexp 1) (point))))
		    method)
	    (setq method (or (function-get (intern-soft function)
								       'lisp-indent-function)
					     (get (intern-soft function) 'lisp-indent-hook)))
	    (cond ((or (eq method 'defun)
				   (and (null method)
					    (> (length function) 3)
					    (string-match "\\`def" function)))
			   (lisp-indent-defform state indent-point))
			  ((integerp method)
			   (lisp-indent-specform method state
							         indent-point normal-indent))
			  (method
			   (funcall method indent-point state))))))))

;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting 
;; ------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'emacs-lisp-mode        
 '(("\\_<[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?"
    .
    'font-lock-number-face)
   ("(\\([/!<>]=\\|[<>=]\\|and\\|or\\|not\\|equal\\|eq[1]?\\)\\_>"
    .
    'font-lock-relation-operator-face)))

;; enable elisp-slime-nav-mode


;; ------------------------------------------------------------------------- ;;
;; Commands
;; ------------------------------------------------------------------------- ;;
(defun elisp-eval ()
  (if (use-region-p)
      (progn 
	    (call-interactively     'eval-region)
        (goto-char (region-end))
        (deactivate-mark))
    
    (progn
      (call-interactively       'eval-defun)
      (end-of-defun)))
  (buffer-goto-end "*Messages*"))

(defun elisp-eval-insert ()
  (unless (use-region-p)
    (mark-defun))
  (let* ((region-contents-string
          (buffer-substring-no-properties
           (region-beginning) (region-end)))
         (value (eval (read region-contents-string))))
    (goto-char (region-end))
    (deactivate-mark)
    
    (unless (eq (point) (point-at-bol))
      (newline-and-indent))      
    (insert (format ";; %S\n" value)))
  
  (buffer-goto-end "*Messages*"))

(defun elisp-set-repl ()
  (--first
   (eq (buffer-local-value 'major-mode it)
       repl-interactive-mode)
   (buffer-list)))

(defun elisp-setup-repl-frame ()
  (save-excursion
    (let ((current-frame (frame-get))
          (buffer (generate-new-buffer-name "*ielm*")))
      (ielm buffer)
      (let* ((window (selected-window))
             (ielm-window-height (/ (window-body-height) -3))
             (ielm-window (split-window nil ielm-window-height)))
        (set-window-buffer window "*Messages*")
        (set-window-dedicated-p window 1)
        (set-window-dedicated-p ielm-window 1)
        (-each
            (-filter #'frame-one-window (frame-list-with-buffer "*Messages*"))
          #'frame-kill)
        (select-window ielm-window))
      (frame-display current-frame)
      buffer)))

(defun elisp-help-variable ()
  "Combines `describe-variable', `describe-function', and `describe-face' into one.  Additionally does this instantaneously and applies display-*Help*-frame (correct formatting)."
  (interactive)
  (let ((calling-frame (frame-get))
	    fn)

    (cond
	 ((not (eq 0 (setq fn (variable-at-point))))
	  (describe-variable fn))
	 ((setq fn (function-called-at-point))
	  (describe-function fn))
	 ((setq fn (face-at-point t))
	  (describe-face fn))
	 ((message "No variable, function, or face at point")))    
    
    (raise-frame calling-frame)))

(defun elisp-help-function ()
  "Combines `describe-variable', `describe-function', and `describe-face' into one.  Additionally does this instantaneously and applies display-*Help*-frame (correct formatting)."
  (interactive)
  (let ((calling-frame (frame-get))
	    fn)

    (cond
	 ((setq fn (function-called-at-point))
	  (describe-function fn))
     ((not (eq 0 (setq fn (variable-at-point))))
	  (describe-variable fn))
	 ((setq fn (face-at-point t))
	  (describe-face fn))
	 ((message "No variable, function, or face at point")))    
    
    (raise-frame calling-frame)))



(provide 'config-emacs-lisp)
;;; CONFIG-EMACS-LISP.EL ends here

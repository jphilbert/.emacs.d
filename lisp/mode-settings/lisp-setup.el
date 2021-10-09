;; ----------------------------------------------------------------------------
;; LISP Mode Setup
;; ----------------------------------------------------------------------------
(provide 'lisp-setup)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook		'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook			'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook	'my-lisp-mode-hook)

(defun my-lisp-mode-hook ()
  (interactive)
  (hs-minor-mode t)
  (hs-hide-all)  
  (flyspell-prog-mode)
  (turn-on-auto-fill)  
  (rainbow-delimiters-mode 1)
  
  (setq-local
   lisp-indent-function #'keyword-fix-lisp-indent-function)
  ) 

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys		emacs-lisp-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]  'elisp-eval
  
  ;; ---------- Indent / Tabs ----------
  (kbd "<tab>")	'indent-for-tab-command   

  ;; ---------- Help ----------
  "\C-hf"			'describe-variable-or-function
  "\C-hv"			'describe-variable-or-function
  [(f1)]			'(lambda ()
				   (interactive)
				   (google-query-at-point t "emacs "))
  [(S-f1)]		'(lambda ()
				   (interactive)
				   (google-query-at-point nil "emacs "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "emacs "))

  ;; ---------- Frame Switching ----------
  [(f12)]           'switch-frame-current-message
  ;; [(S-f12)]           'ielm

  )


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun elisp-eval ()
  "Evaluate a elisp defun or region and step"
  (interactive)
  (if (and transient-mark-mode mark-active)
	 (call-interactively     'eval-region)
    (call-interactively       'eval-defun))
  (deactivate-mark)    
  (end-of-defun))

(defun switch-frame-next-message ()
  "Cycle through message and backtrace buffers."
  (interactive)
  (switch-frame-next-buffer '("\\*Message" "\\*Backtrace") '("^ "))
  (goto-char (point-max)))

(defun switch-frame-current-message ()
  "Switch to current message buffer and move cursor to the end."
  (interactive)
  (with-current-buffer "*Messages*"
    (end-of-buffer-all))
  (display-buffer "*Messages*"))

(defun describe-variable-or-function ()
  "Combines `describe-variable', `describe-function', and `describe-face' into one.  Additionally does this instantaneously and applies display-*Help*-frame (correct formatting)."
  (interactive)
  (let ((calling-frame (get-frame))
	   fn)

    (cond
	((not (eq 0 (setq fn (variable-at-point))))
	 (describe-variable fn))
	((setq fn (function-called-at-point))
	 (describe-function fn))
	((setq fn (face-at-point t))
	 (describe-face fn))
	((message "No variable, function, or face at point")))    
    
    (raise-frame calling-frame)))	; Switch back to calling frame

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
 '(("\\_<-?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?"
    .
    'font-lock-number-face)
   ("(\\([/!<>]=\\|[<>]\\|and\\|or\\|not\\|eq[1]?\\)\\b"
     .
     'font-lock-relation-operator-face)))


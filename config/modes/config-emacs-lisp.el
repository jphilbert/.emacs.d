;;; CONFIG-EMACS-LISP.EL --- config for Elisp programming.

;; This file is not part of GNU Emacs.

;;; Code:

(require 'config-lisp)
(require 'crux)
(require 'rainbow-mode)

(defun config-mode-emacs-lisp ()
  "Sensible defaults for `emacs-lisp-mode'."
  (config-mode-lisp)
  (hs-minor-mode t)
  (hs-hide-all)  
  (eldoc-mode +1)
  (rainbow-mode +1)

  (setq-local
   lisp-indent-function #'keyword-fix-lisp-indent-function)
  
  (setq mode-name "eLISP")

  ;; Recompile your elc when saving an elisp file.
  (add-hook
   'after-save-hook
   (lambda ()
     (when (and
            (string-prefix-p
		   config-root
		   (file-truename buffer-file-name))
            (file-exists-p
		   (byte-compile-dest-file buffer-file-name)))
       (emacs-lisp-byte-compile)))
   nil
   t))

(add-hook 'emacs-lisp-mode-hook 'config-mode-emacs-lisp)

;; ielm is an interactive Emacs Lisp shell
(defun config-mode-emacs-lisp-interactive ()
  "Sensible defaults for `ielm'."
  (config-mode-lisp-interactive)
  (eldoc-mode +1))

(add-hook 'ielm-mode-hook 'config-mode-emacs-lisp-interactive)



;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys		emacs-lisp-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]  'elisp-eval
  
  ;; ---------- Indent / Tabs ----------
  (kbd "<tab>")	'indent-for-tab-command   

  ;; ---------- Help ----------
  "\C-hf"			'elisp-help-function
  "\C-hv"			'elisp-help-variable
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
  [(f12)]           'elisp-frame-messages
  ;; [(S-f12)]           'ielm

  )

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

(provide 'config-emacs-lisp)

;;; CONFIG-EMACS-LISP.EL ends here

;; ------------------------------------------------------------------------- ;;
;; REPL MODE Stuff
;; ------------------------------------------------------------------------- ;;
(defun elisp-eval ()
  "Evaluate a elisp defun or region and step"
  (interactive)
  (if (and transient-mark-mode mark-active)
	 (call-interactively     'eval-region)
    (call-interactively       'eval-defun))
  (deactivate-mark)    
  (end-of-defun))

(defun elisp-frame-messages ()
  "Switch to current message buffer and move cursor to the end."
  (interactive)
  (with-current-buffer "*Messages*"
    (end-of-buffer-all))
  (display-buffer "*Messages*"))

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

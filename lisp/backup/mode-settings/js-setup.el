;; ----------------------------------------------------------------------------
;; Java Script Setup
;; ----------------------------------------------------------------------------
(provide 'js-setup)
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-bounce-indent-p t)
;; Doesn't work well with <return> reindent-then-newline-and-indent

(add-hook 'js-mode-hook		'my-javascript-mode-hook)
(add-hook 'js2-mode-hook	'my-javascript-mode-hook)
(defun my-javascript-mode-hook ()
  ;; (interactive)				; For Debugging

  (skewer-mode)

  ;; (flyspell-prog-mode)

  (hs-minor-mode t)
  (turn-on-auto-fill)

  ;; (ac-js2-mode)
  ;; (setq ac-js2-evaluate-calls t)
  
  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(return)]		'reindent-then-newline-and-indent
   [(M-return)]		'js2-line-break
   
   ;; ---------- Evaluation ----------
   [(shift return)]	'js-eval

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "javascript "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "javascript "))

   ;; ---------- Frame Switching ----------
   [(f12)]              'skewer-repl
   [S-f12]              'list-skewer-clients
   [C-f12]              'restart-httpd
   ))


(add-hook 'skewer-repl-mode-hook	'my-inferior-js-mode-hook)
(defun my-inferior-js-mode-hook ()
  (text-scale-set -1.1)
  
  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   ;; ---------- Input / Prompt Scrolling ----------
   [C-up]               'comint-previous-prompt
   [C-down]             'comint-next-prompt
   [up]                 'comint-previous-matching-input-from-input
   [down]               'comint-next-input

   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "javascript "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "javascript "))

   ;; "\C-hf"      	'R-object-help
   ;; "\C-hv"      	'R-object-str
   ;; "\C-ho"      	'R-object-summaries
   ;; "\C-hn"      	'R-object-names
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-previous-js
   [S-f12]              'list-skewer-clients
   [C-f12]              'restart-httpd
   )
  )


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun js-eval ()
  "Evaluates Java Script code."
  (interactive)
  ;; Pre Evaluation
  (unless (switch-frame-buffer-list '("\\*skewer-repl.*") '("^ "))
    (httpd-start)
    (message "HTTPD Started")
    (sleep-for 0.5))
  
  ;; Evaluation
  (if skewer-clients
	(if (and transient-mark-mode mark-active)
	    (js-eval-region)
	  (progn	
	    (right-char)		; Fixes if at beginning of line
	    (skewer-eval-defun)
	    (goto-char (caddr (skewer-get-defun)))
	    (next-non-blank-line)))
    (message "ERROR: No Skewer Clients"))
  (skewer-repl)
  (switch-frame-previous))

(defun js-eval-region ()
  "Evaluates Java Script region and returns back to current frame."
  (interactive)
  (skewer-eval (buffer-substring (region-beginning) (region-end))
		   #'skewer-post-minibuffer)
  (deactivate-mark))

(defun restart-httpd ()
  "Stop and start the HTTPD"
  (interactive)
  (httpd-stop)
  (sleep-for 0.5)
  (httpd-start)
  (sleep-for 0.5))

(defun switch-frame-previous-js ()
  "Switch to previous Java Script buffer."
  (interactive)
  (if (>= (length (switch-frame-buffer-list '("\\.js$") '("^ "))) 1)
      (switch-frame-previous-buffer '("\\.js$") '("^ "))
    (switch-frame-previous)))



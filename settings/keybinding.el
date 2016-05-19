;; -----------------------------------------------------------------------------
;; KEYBINDING.EL --- General Key Binding
;; -----------------------------------------------------------------------------
;; Filename:		KEYBINDING.EL
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2012-02-13 20:19:37
;; Last-Updated:	2014-04-02
;;           By:	John P. Hilbert
;; Compatibility:	GNU Emacs 23.2.1
;;
;; Features that might be required by this library:
;; <NONE>

;; !!!This file is NOT part of GNU Emacs!!!

(provide 'keybinding)

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
;; <NONE>


;; -----------------------------------------------------------------------------
;; Auto Pairing
;; -----------------------------------------------------------------------------
(autopair-global-mode)				; Better than electric-pair

;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------
(defun global-set-many-keys (&rest key-func-pair)
  "Small function that works like setq to set many keys at once.
It is assumed that the arguments are of the form KEY FUNCTION.
If function is nil the key is unset."
  (while key-func-pair
    (let ((k (pop key-func-pair))
          (f (pop key-func-pair)))
      
      (if (not f)
          (global-unset-key k)
        (global-set-key k f)))))

(defun local-set-many-keys (&rest key-func-pair)
  "Small function that works like setq to set many keys at once.
It is assumed that the arguments are of the form KEY FUNCTION.
If function is nil the key is unset."
  (while key-func-pair
    (let ((k (pop key-func-pair))
          (f (pop key-func-pair)))
      
      (if (not f)
          (local-unset-key k)
        (local-set-key k f)))))

(defun define-many-keys (m &rest key-func-pair)
  "Defines many keys to MAP"
  (while key-func-pair
    (let ((k (pop key-func-pair))
          (f (pop key-func-pair)))
	 (define-key m k f))))

;; --------------------------------------------------------------------------
;; Setting Keys
;; --------------------------------------------------------------------------
(global-set-many-keys
 (kbd "RET")            'reindent-then-newline-and-indent
 (kbd "<C-tab>")        'tab-to-tab-stop

 ;; ---------- File ----------
 (kbd "C-s")                    'save-buffer
 (kbd "C-S-s")                  'write-file
 (kbd "C-o")                    'find-file
 (kbd "C-x d")					'dired-other-frame

 ;; ---------- Find / Replace ----------
 "\C-f"                         'isearch-forward
 (kbd "C-S-f")                  'isearch-backward
 "\C-r"                         'query-replace
 (kbd "C-S-r")                  'replace-regexp
 
 ;; ---------- Miscellaneous ----------
 [(f1)]                 'ac-show-quick-help
 [(f2)]                 'occur
 [(f3)]			    'fold-dwim-toggle
 [(shift f3)]           'fold-dwim-toggle-all
 [(f4)]			    'ispell-word
 [(f5)]                 'linum-mode
 [(f6)]                 'explorer

 (kbd "S-<tab>")	'auto-complete
 
 ;; ---------- Buffers ----------
					; Cycle File Buffers
 [(f11)]                 'switch-frame-previous
 [(f10)]                 'switch-frame-next
					; Cycles all buffers
 [(shift f11)]		'(lambda () (interactive)
			   (switch-frame-previous-buffer
			    '(".*")
			    '("\\*\\*\\*\\*")
			    t))
 [(shift f10)]		'(lambda () (interactive)
			   (switch-frame-next-buffer
			    '(".*")
			    '("\\*\\*\\*\\*")
			    t))
 
 (kbd   "C-x k")                'kill-buffer-or-emacs
 (kbd   "C-S-b")                'bs-show
 (kbd   "C-b")                  'display-buffer-other-frame

					; Toggle between mini-buffer
 [M-f1]			(ti::definteractive
			 (if (window-minibuffer-p (selected-window))
			     (select-window (get-largest-window))
			   (select-window (minibuffer-window))))

 ;; ---------- Moving Frames ----------
 (kbd   "<M-up>")               'move-frame-up
 (kbd   "<M-left>")             'move-frame-left
 (kbd   "<M-right>")            'move-frame-right
 (kbd   "<M-down>")             'move-frame-down
 
 ;; ---------- Killing / Yanking ----------
 (kbd   "M-z")                  'tinyeat-kill-buffer-lines-main
 (kbd   "M-k")                  'tinyeat-kill-line-backward
 (kbd   "C-S-k")                'tinyeat-zap-line
 (kbd   "<M-backspace>")        'tinyeat-backward-preserve
 (kbd   "<S-backspace>")        'tinyeat-delete-whole-word
 (kbd   "M-d")                  'tinyeat-forward-preserve
 (kbd   "C-M-d")                'tinyeat-delete-paragraph
 (kbd   "C-S-y")                'tinyeat-yank-overwrite
 (kbd   "C-S-j")                'tinyeat-join-lines
 
 ;; ---------- Commenting ----------
 (kbd   "M-;")                  'comment-dwim-line 
 (kbd   "M-C-;")                'comment-dwim 
 

 ;; ---------- Expand Regions ----------
 (kbd "C-=")			'er/expand-region	; C +
 (kbd "C--")			'er/contract-region	; C -
 (kbd "S-SPC")			'er/expand-region

 ;; ---------- Help ----------
 (kbd "C-h g")			'websearch-google

 
 ;; ********** Deletions **********
 (kbd   "C-h C-f")              nil
 )


;; --------------------------------------------------------------------------
;; Comint Mode
;; --------------------------------------------------------------------------
(require 'comint)
(define-key comint-mode-map [C-down]
  'comint-next-prompt)
(define-key comint-mode-map [C-up]
  'comint-previous-prompt)

;; These are nice (forget about previous/next-input)
(define-key comint-mode-map [down]
  'comint-next-matching-input-from-input)
(define-key comint-mode-map [up]
  'comint-previous-matching-input-from-input)


;; --------------------------------------------------------------------------
;; Mini Buffer
;; --------------------------------------------------------------------------
;; Remaps up/down to mini-buffer history cycling again
(dolist (map (append (list minibuffer-local-completion-map
			   minibuffer-local-must-match-map)
		     (when (boundp 'minibuffer-local-filename-completion-map)
		       (list minibuffer-local-filename-completion-map))))
  (define-key map [up] 'previous-history-element)
  (define-key map [down] 'next-history-element))

;; Stops accidentally clicking Mini-Buffer and opening *messages*
(define-key minibuffer-inactive-mode-map [mouse-1] nil)

;; --------------------------------------------------------------------------
;; Isearch Fix
;; --------------------------------------------------------------------------
(define-key isearch-mode-map "\C-f"             'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f")      'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-s")        'save-buffer)
(define-key isearch-mode-map (kbd "C-S-s")      'write-file)


;; --------------------------------------------------------------------------
;; Help
;; --------------------------------------------------------------------------
(require 'man)
(define-key Man-mode-map		"q" 'kill-buffer-or-emacs)
(define-key help-mode-map	"q" 'kill-buffer-or-emacs)
(define-key ess-help-mode-map	"q" 'kill-buffer-or-emacs)


;; --------------------------------------------------------------------------
;; Comint Mode
;; --------------------------------------------------------------------------
(require 'comint)
(define-many-keys comint-mode-map
  [C-down]		'comint-next-prompt
  [C-up]			'comint-previous-prompt
  ;; These are nice (forget about previous/next-input)
  [down]			'comint-next-matching-input-from-input
  [up]			'comint-previous-matching-input-from-input
  [S-C-up]		'previous-line
  [S-C-down]		'next-line
  
  ;; ---------- Help ----------
  (kbd "C-h f")   	'man-at-point 
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "bash "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "bash "))

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-previous
  [S-f12]              'shell-new
  )


;; --------------------------------------------------------------------------
;; SQL
;; --------------------------------------------------------------------------
(define-many-keys sql-interactive-mode-map
  ;; ---------- Input / Prompt Scrolling ----------
  [C-up]               'comint-previous-prompt
  [C-down]             'comint-next-prompt
  [up]                 'comint-previous-input
  [down]               'comint-next-input
  [S-C-up]			'previous-line
  [S-C-down]			'next-line
  

  ;; ---------- Completion ----------
  (kbd "<tab>")	'completion-at-point

  ;; ---------- Help ----------
  [(S-f1)]	  	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
  ;; "\C-hf"              'sql-tables
  ;; "\C-he"              'sql-explain
  "\C-hv"              'sql-describe
  "\C-hs"              'sql-show-table

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-next-sql
  [S-f12]              'sql-connect
  )

(define-many-keys sql-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]     'sql-eval

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'sql-fix-indent

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-sql
  [S-f12]              'sql-connect
  [C-f12]              'sql-set-sqli-buffer

  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
  ;; "\C-hf"              'sql-tables
  ;; "\C-he"              'sql-explain
  "\C-hv"              'sql-describe
  "\C-hs"              'sql-show-table
  )


;; --------------------------------------------------------------------------
;; R
;; --------------------------------------------------------------------------
(define-many-keys ess-mode-map
  ;; [(return)]		'newline-and-indent

  ;; ---------- Evaluation ----------
  [(shift return)]     'R-eval
  

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'indent-for-tab-command
  
  
  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "R "))

  "\C-hf"      	'R-object-help
  "\C-hv"      	'R-object-str
  "\C-ho"      	'R-object-summaries
  "\C-hn"      	'R-object-names
  "\C-hV"      	'ess-display-vignettes
  "\C-hH"      	'ess-handy-commands

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-R
  [S-f12]              'R-process-new
  [C-f12]              'ess-switch-process
  )

(define-many-keys inferior-ess-mode-map
  ;; ---------- Input / Prompt Scrolling ----------
  [C-up]               'comint-previous-prompt
  [C-down]             'comint-next-prompt
  [up]                 'comint-previous-input
  [down]               'comint-next-input
  [S-C-up]			'previous-line
  [S-C-down]			'next-line

  
  ;; ---------- Completion ----------
  ;; (kbd "<tab>")	'completion-at-point


  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "R "))

  "\C-hf"      	'R-object-help
  "\C-hv"      	'R-object-str
  "\C-ho"      	'R-object-summaries
  "\C-hn"      	'R-object-names
  "\C-hV"      	'ess-display-vignettes
  "\C-hH"      	'ess-handy-commands
  
  ;; ---------- Frame Switching ----------
  [(f12)]          'switch-frame-previous
  [S-f12]		'R-process-new
  )


;; --------------------------------------------------------------------------
;; Python
;; --------------------------------------------------------------------------
(require 'python)
(define-many-keys python-mode-map
  [(return)]		'newline-and-indent
  
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
  (kbd "C-h f")   	'anaconda-mode-view-doc
  
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
  (kbd "C-h f")   	'anaconda-mode-view-doc

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-previous
  )


;; --------------------------------------------------------------------------
;; Elisp
;; --------------------------------------------------------------------------
(define-many-keys emacs-lisp-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]     'elisp-eval
  
  ;; ---------- Indent / Tabs ----------
  (kbd "C-<tab>")	'tab-to-tab-stop-magic
  (kbd "<tab>")        'indent-for-tab-command   

  ;; ---------- Help ----------
  "\C-hf"      	'describe-variable-or-function
  [(S-f1)]		'(lambda ()
				   (interactive)
				   (google-query-at-point t "emacs "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "emacs "))

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-message

  )


;; --------------------------------------------------------------------------
;; Markdown
;; --------------------------------------------------------------------------
(require 'markdown-mode)
(define-many-keys markdown-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]		'markdown-preview
  
  ;; ---------- Styles ----------
  (kbd "C-i")			'markdown-insert-italic
  (kbd "C-b")			'markdown-insert-bold
  (kbd "C-l")			'markdown-insert-link
  (kbd "C-j")			'markdown-insert-code
  

  ;; ---------- Hide-Show ----------
  [(f3)]			     'markdown-hide-subtree
  [(shift f3)]           'markdown-show-subtree
  
  ;; ---------- Help ----------
  "\C-hf"      	'(lambda ()
				   (interactive)
				   (browse-url
   "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")
				   )
  [(S-f1)]		'(lambda ()
				   (interactive)
				   (browse-url
   "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")
				   )
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "markdown "))
  )


;; --------------------------------------------------------------------------
;; Shell
;; --------------------------------------------------------------------------
(define-many-keys ntcmd-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]     'shell-eval

  ;; ---------- Help ----------
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "dos "))
  (kbd "C-h W")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "dos "))

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-shell
  [S-f12]              'shell-new
  [C-f12]              'shell-buffer-choose
  
  ;; ---------- Auto Pairing ----------
  (kbd "(")            'skeleton-pair-insert-maybe
  (kbd "[")            'skeleton-pair-insert-maybe
  (kbd "{")            'skeleton-pair-insert-maybe
  (kbd "\"")           'skeleton-pair-insert-maybe
  (kbd "\'")           'skeleton-pair-insert-maybe
  (kbd "\`")           'skeleton-pair-insert-maybe)  

(define-many-keys shell-mode-map
   ;; ---------- Help ----------
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "dos "))
   (kbd "C-h W")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "dos "))
   
   ;; ---------- Frame Switching ----------
   [(f12)]              'switch-frame-next-shell
   [S-f12]              'shell-new

   ;; ---------- Auto Pairing ----------
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe)


;;; KEYBINDING.EL ends here


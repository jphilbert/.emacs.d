;; -----------------------------------------------------------------------------
;; KEYBINDING.EL --- General Key Binding
;; -----------------------------------------------------------------------------
;; Filename:		KEYBINDING.EL
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2012-02-13 20:19:37
;; Last-Updated:	2018-02-05
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
;; (autopair-global-mode)				; Better than electric-pair
(electric-pair-mode)

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
 [(f1)]				'(lambda ()
					   (interactive)
					   (google-query-at-point t)) 
 [(f2)]                  'occur
 [(f3)]			     'fold-dwim-toggle
 [(shift f3)]            'fold-dwim-toggle-all

 [(f5)]                  'linum-mode
 [(f6)]                  'explorer
 
 [(f7)]				'ispell-word	; MS uses F7 for spell check
 [(shift f7)]			'thesaurus-at-point ; MS uses S-F7 for thesaurus
 [(ctrl shift f7)]		'flyspell-mode

 [(f8)]				(ti::definteractive
					 (shell-command
					  (read-shell-command "Shell command: ") 1))
 [(f9)]				'menu-bar-mode

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
 ;; Broke if main screen is on the right
 ;; (kbd   "<M-up>")               'move-frame-up
 ;; (kbd   "<M-left>")             'move-frame-left
 ;; (kbd   "<M-right>")            'move-frame-right
 ;; (kbd   "<M-down>")             'move-frame-down

 
 ;; ---------- Killing / Yanking ----------
 (kbd   "M-z")                  'tinyeat-kill-buffer-lines-main
 (kbd   "M-k")                  'tinyeat-kill-line-backward
 (kbd   "C-S-k")                'tinyeat-zap-line
 (kbd   "<M-backspace>")        'tinyeat-backward-preserve
 (kbd   "<S-backspace>")        'tinyeat-delete-whole-word
 (kbd   "M-d")                  'tinyeat-forward-preserve
 (kbd   "<C-M-backspace>")	  'tinyeat-delete-paragraph
 (kbd   "C-S-y")                'tinyeat-yank-overwrite
 (kbd   "C-S-j")                'tinyeat-join-lines

 "\M-V"					'yank-pop-forwards
 "\M-v"					'cua-paste-pop ; this doesn't work due to it being
								; key defined my CUA mode (see fix
								; below) 

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
 (kbd   "C-\\")			  nil
 )

(define-key cua--cua-keys-keymap (kbd "M-v") 'cua-paste-pop) ; CUA overwrite


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
  [(f1)]		   	'(lambda ()
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



;; --------------------------------------------------------------------------
;; Markdown-Mode
;; --------------------------------------------------------------------------
(require 'markdown-mode)
(define-many-keys markdown-mode-map
  [(f3)]				'markdown-shifttab
  (kbd "<tab>")		'tab-to-tab-stop)


;;; KEYBINDING.EL ends here


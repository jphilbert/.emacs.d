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



;; ------------------------------------------------------------------------- ;;
;; Functions
;; ------------------------------------------------------------------------- ;;
(defun global-set-keys (&rest key-command)
  "Defines a series of KEY-COMMAND globally similar to `global-set-key'.  It
is assumed that the KEY-COMMAND come in pairs of KEY and COMMAND. If COMMAND is nil the key is unset.

See also `local-set-keys' and `define-keys'"
  (while key-command
    (let ((k (pop key-command))
          (f (pop key-command)))
      
      (if f
		(global-set-key k f)
	   (global-unset-key k)))))

(defun local-set-keys (&rest key-command)
  "Defines a series of KEY-COMMAND locally similar to `local-set-key'.  It
is assumed that the KEY-COMMAND come in pairs of KEY and COMMAND. If COMMAND is nil the key is unset.

See also `global-set-keys' and `define-keys'"
  (while key-command
    (let ((k (pop key-command))
          (f (pop key-command)))
      
      (if f
		(local-set-key k f)
	   (local-unset-key k)))))

(defun define-keys (keymap &rest key-def)
  "Defines a series of KEY-DEF for a KEYMAP similar to `define-key'.  It
is assumed that the KEY-DEF are grouped in pairs of KEY and DEFINITION.

See also `global-set-keys' and `local-set-keys'"
  (while key-def
    (let ((k (pop key-def))
          (f (pop key-def)))
	 (define-key keymap k f))))

(define-obsolete-function-alias
  'global-set-many-keys #'global-set-keys "2021-8-19")
(define-obsolete-function-alias
  'local-set-many-keys #'local-set-keys "2021-8-19")
(define-obsolete-function-alias
  'define-many-keys #'define-keys "2021-8-19")

;; ------------------------------------------------------------------------- ;;
;; Settings
;; ------------------------------------------------------------------------- ;;
(electric-pair-mode)				; turn on auto pairing

;; allows binding "C-[" without breaking "M-"
;; (see https://emacs.stackexchange.com/a/52334)
(define-key input-decode-map 
    (kbd "C-[") 
    [control-bracketleft])

;; CUA overwrite
(define-key cua--cua-keys-keymap (kbd "M-v") 'cua-paste-pop) 

(global-set-many-keys
 (kbd "RET")			'reindent-then-newline-and-indent
 (kbd "<C-tab>")		'tab-to-tab-stop

 ;; ---------- File ----------
 (kbd "C-s")             'save-buffer
 (kbd "C-S-s")           'write-file
 (kbd "C-o")             'find-file
 (kbd "C-x d")			'dired-other-frame

 ;; ---------- Find / Replace ----------
 "\C-f"                  'isearch-forward
 (kbd "C-S-f")           'isearch-backward
 "\C-r"                  'query-replace
 (kbd "C-S-r")           'replace-regexp
 
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

 (kbd "S-<tab>")		'auto-complete
 
 ;; ---------- Buffers ----------
					; Cycle File Buffers
 [(f11)]                 'switch-frame-previous
 [(f10)]                 'get-scratch-buffer
 
 (kbd   "C-x k")         'kill-buffer-or-emacs
 (kbd   "C-S-b")		'bs-show
 (kbd   "C-b")           'display-buffer-other-frame

					; Toggle between mini-buffer
 [M-f1]				(ti::definteractive
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
 (kbd   "M-z")				'tinyeat-kill-buffer-lines-main
 (kbd   "M-k")                'tinyeat-kill-line-backward
 (kbd   "C-S-k")			'tinyeat-zap-line
 (kbd   "<M-backspace>")		'tinyeat-backward-preserve
 (kbd   "<S-backspace>")		'tinyeat-delete-whole-word
 (kbd   "M-d")				'tinyeat-forward-preserve
 (kbd   "<C-M-backspace>")	'tinyeat-delete-paragraph
 (kbd   "C-S-y")			'tinyeat-yank-overwrite
 (kbd   "C-S-j")			'tinyeat-join-lines

 "\M-V"					'yank-pop-forwards
 "\M-v"					'cua-paste-pop ; this doesn't work due to it being
								; key defined my CUA mode (see fix
								; below) 

 ;; ---------- Navigation ----------
 (kbd   "C-]")				'forward-list
 [control-bracketleft]		'backward-list
 (kbd   "C-}")				'down-list
 (kbd   "C-{")				'backward-up-list
 
 
 ;; ---------- Commenting ----------
 (kbd   "M-;")				'comment-dwim-line 
 (kbd   "M-C-;")              'comment-dwim 
 

 ;; ---------- Expand Regions ----------
 (kbd "C-=")				'er/expand-region	; C +
 (kbd "C--")				'er/contract-region	; C -
 (kbd "S-SPC")				'er/expand-region

 ;; ---------- Help ----------
 (kbd "C-h g")				'websearch-google


 ;; ---------- Ctrl-g => Go To ---------- ;;
 (kbd "C-g")				(lookup-key esc-map (kbd "g"))

 ;; ********** Deletions **********
 (kbd   "C-h C-f")			nil
 (kbd   "C-\\")			nil
 )





;; --------------------------------------------------------------------------
;; Comint Mode
;; --------------------------------------------------------------------------
(require 'comint)
(define-many-keys	comint-mode-map
  [C-down]			'comint-next-prompt
  [C-up]				'comint-previous-prompt
  ;; These are nice (forget about previous/next-input)
  [down]				'comint-next-matching-input-from-input
  [up]				'comint-previous-matching-input-from-input
  [S-C-up]			'previous-line
  [S-C-down]			'next-line
  
  ;; ---------- Help ----------
  (kbd "C-h f")   		'man-at-point 
  [(f1)]		   		'(lambda ()
					   (interactive)
					   (google-query-at-point t "bash "))
  (kbd "C-h w")   		'(lambda ()
					   (interactive)
					   (google-query-at-point nil "bash "))

  ;; ---------- Frame Switching ----------
  [(f12)]              	'switch-frame-previous
  [S-f12]              	'shell-new
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
(define-many-keys	isearch-mode-map
  "\C-f"				'isearch-repeat-forward
  (kbd "C-S-f")		'isearch-repeat-backward
  (kbd "C-s")			'save-buffer
  (kbd "C-S-s")		'write-file)


;; --------------------------------------------------------------------------
;; Help
;; --------------------------------------------------------------------------


(define-many-keys	ac-completing-map
  (kbd "<tab>")		'ac-next
  [(return)]			'ac-complete)

(define-many-keys	goto-map
  "l"				'goto-line
  (kbd "<up>")			'bookmark-jump
  (kbd "<down>")		'bookmark-set
  (kbd "<right>")		'bookmark-bmenu-list)

(define-many-keys	bookmark-bmenu-mode-map
  "j"				'bookmark-bmenu-other-window
  "\C-c\C-c"			'bookmark-bmenu-other-window
  "f"				'bookmark-bmenu-other-window
  "\C-m"				'bookmark-bmenu-other-window)

;; (require 'man)
;; (define-key Man-mode-map		"q" 'kill-buffer-or-emacs)

(define-key		help-mode-map
  "q"				'kill-buffer-or-emacs)


;;; KEYBINDING.EL ends here


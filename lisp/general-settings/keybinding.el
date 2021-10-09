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
;; General
;; ------------------------------------------------------------------------- ;;

;; allows binding "C-[" without breaking "M-"
;; (see https://emacs.stackexchange.com/a/52334)
(define-key input-decode-map 
    (kbd "C-[") 
    [control-bracketleft])

;; CUA overwrite
(define-key cua--cua-keys-keymap
  (kbd "M-v") 'cua-paste-pop) 

(global-set-keys
 (kbd "RET")			'reindent-then-newline-and-indent
 (kbd "<C-tab>")		'tab-to-tab-stop-magic

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
 [(shift f9)]			'mode-line-toggle

 (kbd "S-<tab>")		'auto-complete
 
 ;; ---------- Buffers ----------
					; Cycle File Buffers
 [(f11)]                 'switch-frame-previous
 [(f10)]                 'get-scratch-buffer
 
 (kbd   "C-x k")         'kill-buffer-smart
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
								; key defined my CUA mode (see fix)

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
(define-keys	isearch-mode-map
  "\C-f"				'isearch-repeat-forward
  (kbd "C-S-f")		'isearch-repeat-backward
  (kbd "C-s")			'save-buffer
  (kbd "C-S-s")		'write-file
  "\C-v"				'isearch-yank-pop)

;; --------------------------------------------------------------------------
;; Help
;; --------------------------------------------------------------------------
(define-keys	goto-map
  "l"				'goto-line
  (kbd "<up>")			'bookmark-jump
  (kbd "<down>")		'bookmark-set
  (kbd "<right>")		'bookmark-bmenu-list)

(define-keys	bookmark-bmenu-mode-map
  "j"				'bookmark-bmenu-other-window
  "\C-c\C-c"			'bookmark-bmenu-other-window
  "f"				'bookmark-bmenu-other-window
  "\C-m"				'bookmark-bmenu-other-window)

;; (require 'man)
;; (define-key Man-mode-map		"q" 'kill-buffer-or-emacs)

(define-key		help-mode-map
  "q"				'kill-buffer-or-emacs)


;;; KEYBINDING.EL ends here


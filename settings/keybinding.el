;; -----------------------------------------------------------------------------
;; KEYBINDING.EL --- General Key Binding
;; -----------------------------------------------------------------------------
;; Filename:		KEYBINDING.EL
;; Description:		General Key Binding
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

 ;; ---------- Find / Replace ----------
 "\C-f"                         'isearch-forward
 (kbd "C-S-f")                  'isearch-backward
 "\C-r"                         'query-replace
 
 ;; ---------- Miscellaneous ----------
 [(f1)]                 'ac-show-quick-help
 [(f2)]                 'occur
 [(f3)]			'fold-dwim-toggle
 [(shift f3)]           'fold-dwim-toggle-all
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


;; --------------------------------------------------------------------------
;; Isearch Fix
;; --------------------------------------------------------------------------
(define-key isearch-mode-map "\C-f"             'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f")      'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-s")        'save-buffer)
(define-key isearch-mode-map (kbd "C-S-s")      'write-file)



;;; KEYBINDING.EL ends here


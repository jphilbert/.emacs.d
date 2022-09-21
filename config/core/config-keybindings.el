;; -------------------------------------------------------------------------- ;;
;; CONFIG-GLOBAL-KEYBINDINGS.EL --- Global keybindings
;; -------------------------------------------------------------------------- ;;
;; Filename:		KEYBINDING.EL
;; Author:          John P. Hilbert <jphilbert@gmail.com>
;; Created:         2022-08-14
;; Compatibility:	GNU Emacs 27.2
;;
;; !!!This file is NOT part of GNU Emacs!!!

;; -------------------------------------------------------------------------- ;;
;; General
;; -------------------------------------------------------------------------- ;;
(cua-mode)

;; allows binding "C-[" without breaking "M-"
;; (see https://emacs.stackexchange.com/a/52334)
(define-key     input-decode-map 
  (kbd "C-[")           [control-bracketleft])

;; CUA overwrite
;; (define-key cua--cua-keys-keymap
;;   (kbd "M-v") 'cua-paste-pop) 

(global-set-keys
 (kbd "RET")			'reindent-then-newline-and-indent
 (kbd "<C-tab>")		'tab-to-tab-stop-magic

 ;; ---------- File ----------
 (kbd "C-s")            'save-buffer
 (kbd "C-S-s")          'write-file
 (kbd "C-o")            'find-file
 (kbd "C-x d")			'dired-other-frame

 ;; ---------- Find / Replace ----------
 (kbd "C-f")            'isearch-forward
 (kbd "C-S-f")          'isearch-backward
 (kbd "C-r")            'query-replace
 (kbd "C-S-r")          'replace-regexp
 
 ;; ---------- Miscellaneous ----------
 
 (kbd "<f1>")           'google-at-point
 
 (kbd "<f2>")           'occur
 
 (kbd "<f3>")           'hs-toggle
 (kbd "S-<f3>")         'hs-toggle-all

 (kbd "<f5>")           'display-line-numbers-mode
 
 (kbd "<f6>")           'explorer

 ;; Spelling and Thesaurus
 (kbd "<f7>")           'flyspell-correct-at-point
 (kbd "S-<f7>")			'thesaurus-at-point
 (kbd "C-S-<f7>")		'flyspell-correct-wrapper

 (kbd "<f9>")           'menu-bar-mode
 (kbd "S-<f9>")			'mode-line-toggle

 ;; (kbd "S-<tab>")		'auto-complete
 
 ;; ---------- Buffers ----------
 ;; (kbd "<f10>")                'get-scratch-buffer
 (kbd "<f11>")          'frame-get-last
 ;; (kbd "<f12>")          'repl-frame

 (kbd "C-b")            'consult-buffer-other-frame
 (kbd "C-x k")          'frame-kill-dwim
 (kbd "C-S-b")          'bs-show

 
 ;; ---------- Killing / Yanking ----------
 (kbd "<backspace>")    'config-smart-delete-backward
 (kbd "<delete>")       'config-smart-delete-forward
 (kbd "C-S-<backspace>")  'config-smart-delete-line-backward
 (kbd "C-S-<delete>")     'config-smart-delete-line-forward

 ;; "\M-V"                     'yank-pop-forwards
 ;; "\M-v"                     'cua-paste-pop
 ;;                                        ; this doesn't work due to it being
 ;;                                        ; key defined my CUA mode (see fix)

 ;; ---------- Navigation ----------
 (kbd "C-]")				'forward-list
 [control-bracketleft]		'backward-list
 (kbd "C-}")				'down-list
 (kbd "C-{")				'backward-up-list
 
 
 ;; ---------- Commenting ----------
 (kbd "M-;")                'comment-dwim-line 
 (kbd "M-C-;")              'crux-duplicate-and-comment-current-line-or-region
 

 ;; ---------- Expand Regions ----------
 (kbd "C-=")				'er/expand-region	; C +
 (kbd "C--")				'er/contract-region	; C -
 (kbd "S-SPC")				'er/expand-region

 ;; ---------- Help ----------

 ;; ---------- Ctrl-g => Go To ---------- ;;
 (kbd "C-g")				(lookup-key esc-map (kbd "g"))

 ;; ********** Deletions **********
 (kbd "C-h C-f")			nil
 (kbd "C-\\")               nil
 (kbd "M-`")                nil
 )

;; (("C-M-f" . sp-forward-sexp)
;;  ("C-M-b" . sp-backward-sexp)
;;  ("C-M-d" . sp-down-sexp)
;;  ("C-M-a" . sp-backward-down-sexp)
;;  ("C-S-d" . sp-beginning-of-sexp)
;;  ("C-S-a" . sp-end-of-sexp)
;;  ("C-M-e" . sp-up-sexp)
;;  ("C-M-u" . sp-backward-up-sexp)
;;  ("C-M-n" . sp-next-sexp)
;;  ("C-M-p" . sp-previous-sexp)
;;  ("C-M-k" . sp-kill-sexp)
;;  ("C-M-w" . sp-copy-sexp)
;;  ("M-<delete>" . sp-unwrap-sexp)
;;  ("M-<backspace>" . sp-backward-unwrap-sexp)
;;  ("C-<right>" . sp-forward-slurp-sexp)
;;  ("C-<left>" . sp-forward-barf-sexp)
;;  ("C-M-<left>" . sp-backward-slurp-sexp)
;;  ("C-M-<right>" . sp-backward-barf-sexp)
;;  ("M-D" . sp-splice-sexp)
;;  ("C-M-<delete>" . sp-splice-sexp-killing-forward)
;;  ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
;;  ("C-S-<backspace>" . sp-splice-sexp-killing-around)
;;  ("C-]" . sp-select-next-thing-exchange)
;;  ("C-M-]" . sp-select-next-thing)
;;  ("C-M-SPC" . sp-mark-sexp)
;;  ("M-F" . sp-forward-symbol)
;;  ("M-B" . sp-backward-symbol))

;; (("C-M-f" . sp-forward-sexp)
;;  ("C-M-b" . sp-backward-sexp)
;;  ("C-M-u" . sp-backward-up-sexp)
;;  ("C-M-d" . sp-down-sexp)
;;  ("C-M-p" . sp-backward-down-sexp)
;;  ("C-M-n" . sp-up-sexp)
;;  ("M-s" . sp-splice-sexp)
;;  ("M-<up>" . sp-splice-sexp-killing-backward)
;;  ("M-<down>" . sp-splice-sexp-killing-forward)
;;  ("M-r" . sp-splice-sexp-killing-around)
;;  ("M-(" . sp-wrap-round)
;;  ("C-)" . sp-forward-slurp-sexp)
;;  ("C-<right>" . sp-forward-slurp-sexp)
;;  ("C-}" . sp-forward-barf-sexp)
;;  ("C-<left>" . sp-forward-barf-sexp)
;;  ("C-(" . sp-backward-slurp-sexp)
;;  ("C-M-<left>" . sp-backward-slurp-sexp)
;;  ("C-{" . sp-backward-barf-sexp)
;;  ("C-M-<right>" . sp-backward-barf-sexp)
;;  ("M-S" . sp-split-sexp)
;;  ("M-j" . sp-join-sexp)
;;  ("M-?" . sp-convolute-sexp))

;; Make the GUI close (X) button act like kill-buffer
(defadvice handle-delete-frame (around delete-frame-after-kill activate)
  "Map (X) button to kill-buffer"
  (let ((frame   (posn-window (event-start event))))
    (frame-kill-dwim)))



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
  (kbd "C-f")           'isearch-repeat-forward
  (kbd "C-S-f")         'isearch-repeat-backward
  (kbd "C-s")           'save-buffer
  (kbd "C-S-s")         'write-file
  (kbd "C-v")           'isearch-yank-pop)

;; --------------------------------------------------------------------------
;; Help
;; --------------------------------------------------------------------------
(eval-when-compile (require 'bookmark))
(define-keys	goto-map
  (kbd "l")				'goto-line
  (kbd "<up>")			'bookmark-jump
  (kbd "<down>")		'bookmark-set
  (kbd "<right>")		'bookmark-bmenu-list)

(define-keys	bookmark-bmenu-mode-map
  (kbd "j")				'bookmark-bmenu-other-window
  (kbd "C-c C-c")		'bookmark-bmenu-other-window
  (kbd "f")				'bookmark-bmenu-other-window
  (kbd "C-m")			'bookmark-bmenu-other-window)

;; (require 'man)
;; (define-key Man-mode-map		"q" 'kill-buffer-or-emacs)

(define-key		help-mode-map
  (kbd "q")				'frame-kill-buffer)


(define-keys    tempel-map
  (kbd "SPC")           'corfu-insert-separator)

(define-keys    vertico-map
  (kbd "RET")           'vertico-directory-enter
  (kbd "DEL")           'vertico-directory-delete-char
  (kbd "M-DEL")         'vertico-directory-delete-word)

(define-keys    minibuffer-local-map
  (kbd "M-a")           'marginalia-cycle)

(define-keys    corfu-map
  (kbd "SPC")           'corfu-insert-separator)

;; (define-key prog-mode-map (kbd "M-(") (config-wrap-with "("))
;; ;; FIXME: pick terminal friendly binding
;; ;; (define-key prog-mode-map (kbd "M-[") (config-wrap-with "["))
;; (define-key prog-mode-map (kbd "M-\"") (config-wrap-with "\""))

(smartrep-define-key global-map "C-c ."  
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(provide 'config-keybindings)

;;; CONFIG-KEYBINDINGS.EL ends here

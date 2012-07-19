;; -----------------------------------------------------------------------------
;; KEYBINDING.EL --- General Key Binding
;; -----------------------------------------------------------------------------
;; Filename:		KEYBINDING.EL
;; Description:		General Key Binding
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Copyright (C)	2012, John P. Hilbert, all rights reserved.
;; Created:		2012-02-13 20:19:37
;; Version:		0.1
;; Last-Updated:	2012-02-13 20:19:37
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 23.2.1
;;
;; Features that might be required by this library:
;; <NONE>

;; !!!This file is NOT part of GNU Emacs!!!

;; -----------------------------------------------------------------------------
;; License
;; -----------------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; General Key Binding

;; 
;;

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put KEYBINDING.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'keybinding)
;;	

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------
;; <NONE>
;;
;; All of the above can customize by:
;;      M-x customize-group RET keybinding RET
;;

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2012-02-13
;;      * First released.
;;2012-03-14
;;	* added function 'today'
;;

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
;; <NONE>

;; -----------------------------------------------------------------------------
;; Code:
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

(global-set-many-keys
 (kbd "RET")            'reindent-then-newline-and-indent
 (kbd "<C-tab>")        'tab-to-tab-stop
 
 
 
 ;; Search with C-f / C-F (control-maj-F for backware search)
 "\C-f"                         'isearch-forward
 (kbd "C-S-f")                  'isearch-backward
 ;; Save with C-s / C-S
 (kbd "C-s")                    'save-buffer
 (kbd "C-S-s")                  'write-file
 ;; While we are at it change open file C-o
 (kbd "C-o")                    'find-file
 ;; ...and Replace (don't know if we need to change any mode-maps)
 "\C-r"                         'query-replace)

;; need to redefine them for isearch mode (don't know why)
(define-key isearch-mode-map "\C-f"             'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f")      'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-s")        'save-buffer)
(define-key isearch-mode-map (kbd "C-S-s")      'write-file)

;; ---------- Buffers / Windows ----------
(windmove-default-keybindings)          ; S-arrow = move to window
(global-set-many-keys
 [(f5)]                 'switch-frame-next         
 [(f6)]                 'switch-frame-previous
 
 [(f10)]                'switch-frame-next-shell
 [(f11)]                'switch-frame-next-help
 
 (kbd   "C-h C-f")              nil
 ;; Toggle between minibuffer
 [f4]                         (ti::definteractive
                               (if (window-minibuffer-p (selected-window))
                                   (select-window (get-largest-window))
                                 (select-window (minibuffer-window))))
 ;; Close Emacs
 [M-f4]				'save-buffers-kill-terminal
 (kbd   "C-x k")                'kill-buffer-or-emacs
 (kbd   "C-S-b")                'bs-show
 (kbd   "C-b")                  'display-buffer-other-frame
 
 ;; (kbd        "<f6>")                 'last-frame
 ;; (kbd        "<f12>")                'toggle-terminal
 
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
 
 ;; ---------- Folding ----------
 (kbd   "<f3>")                 'fold-dwim-toggle
 [(shift f3)]                   'fold-dwim-toggle-all)

;; --------------------------------------------------------------------------
;; Comint Mode
;; --------------------------------------------------------------------------
(define-key comint-mode-map
  [C-down]
  'comint-next-prompt)
(define-key comint-mode-map
  [C-up]
  'comint-previous-prompt)

;; These are nice (forget about previous/next-input)
(define-key comint-mode-map
  [down]
  'comint-next-matching-input-from-input)
(define-key
  comint-mode-map
  [up]
  'comint-previous-matching-input-from-input)

(defun today ()
  "Insert string for today's date nicely formatted"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(provide 'keybinding)

;;; KEYBINDING.EL ends here
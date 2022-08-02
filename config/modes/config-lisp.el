;;; CONFIG-LISP.EL --- Configuration common to all LISP modes.

;; This file is not part of GNU Emacs.

;;; Code:
(require 'config-programming)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-lisp ()
  )

(defun config-mode-lisp-interactive ()
  )

;; (add-hook 'lisp-mode 'config-mode-lisp)
;; (add-hook 'lisp-interaction-mode 'config-mode-lisp-interactive)

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; Smart Parenthesis wrapping keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (config-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-[") (config-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (config-wrap-with "\""))


;; --------------------------------------------------------------------------
;; Syntax Highlighting
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------


(provide 'config-lisp)
;;; CONFIG-LISP.EL ends here

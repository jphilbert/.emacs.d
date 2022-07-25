;;; CONFIG-LISP.EL --- Configuration common to all LISP modes.

;; This file is not part of GNU Emacs.

;;; Code:
(require 'config-programming)

(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; Smart Parenthesis wrapping keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (config-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-[") (config-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (config-wrap-with "\""))

(defun config-mode-lisp ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(defun config-mode-lisp-interactive ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(provide 'config-lisp)


;;; CONFIG-LISP.EL ends here

;; (use-package lisp-setup
;;   :hook (
;; 	    (lisp-mode				. my-lisp-mode-hook)
;; 	    (lisp-interaction-mode	. my-lisp-mode-hook)))

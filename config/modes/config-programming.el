;;; CONFIG-PROGRAMMING.EL --- prog-mode configuration

;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

;;; Code:


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-programming ()
  "Default coding hook, useful with any programming language."

  ;; Spelling
  (flyspell-prog-mode)
  
  ;; Smart Parenthesis
  (smartparens-mode +1)
  (smartparens-global-strict-mode -1)

  ;; Color Delimiters
  (rainbow-delimiters-mode +1)
  
  ;; Auto fill comments only
  (auto-fill-only-comments-local)
  
  ;; Hide-Show
  (hs-minor-mode)
  
  ;; Turn URLs into links
  (goto-address-mode)
  )

(add-hook 'prog-mode-hook 'config-mode-programming)


;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-keys prog-mode-map
  (kbd "<backspace>")       'smart-hungry-delete-backward-char
  (kbd "<delete>")          'smart-hungry-delete-forward-char)

;; --------------------------------------------------------------------------
;; Syntax Coloring
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------


(provide 'config-programming)
;;; CONFIG-PROGRAMMING.EL ends here

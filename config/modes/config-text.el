
;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-text ()
  "Default coding hook, useful with any programming language."

  ;; Spelling
  (flyspell-mode)
  
  ;; Auto fill
  (auto-fill-mode +1)
  
  ;; Turn URLs into links
  (goto-address-mode)

;; (add-hook 'text-mode-hook 'config-enable-whitespace)
;; (add-hook 'text-mode-hook 'abbrev-mode)
  )

(add-hook 'text-mode 'config-mode-text)


;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;; Syntax Highlighting
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------


(provide 'config-text)
;;; CONFIG-TEXT.EL ends here

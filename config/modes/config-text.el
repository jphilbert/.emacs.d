
;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-text ()
  "Default text hook for non-programming modes"

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
(define-keys		text-mode-map
  (kbd "RET")			'newline-and-indent)

;; --------------------------------------------------------------------------
;; Syntax Highlighting
;; --------------------------------------------------------------------------


;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------


(provide 'config-text)
;;; CONFIG-TEXT.EL ends here

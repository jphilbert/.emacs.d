;;; CONFIG-YAML.EL --- Emacs Config: YAML programming support.

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-yaml ()
  "Default coding hook, useful with any programming language."
  (whitespace-mode)
  (subword-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'yaml-mode-hook 'config-mode-yaml)


;; --------------------------------------------------------------------------
;; Key Binding
;; --------------------------------------------------------------------------
;;   (:map yaml-mode-map
;;        ("\C-m"   . newline-and-indent)))


(provide 'config-yaml)
;;; CONFIG-YAML.EL ends here



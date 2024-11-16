;; ========================================================================== ;;
;; CONFIG-JS.EL --- configuration for JavaScript modes                        ;;
;; ========================================================================== ;;
;; This file is not part of GNU Emacs.

;; -------------------------------------------------------------------------- ;;
;; Global Settings                                                            ;;
;; -------------------------------------------------------------------------- ;;
;; (config-require-packages '(js2-mode json-mode))
(require 'config-programming)
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.gs\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js-indent-level 2)



;; -------------------------------------------------------------------------- ;;
;; Hooks                                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defun config-mode-js ()
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (setq mode-name "JScript")
  (js2-imenu-extras-mode +1)
  (subword-mode +1)
  )


(add-hook 'js2-mode-hook                    'config-mode-js)



(provide 'config-js)
;;; prelude-js.el ends here

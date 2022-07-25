;;; CONFIG-CSS.EL --- css support

;;; Commentary:

;; Some basic configuration for css-mode.

;;; Code:

(with-eval-after-load 'css-mode
  (require 'rainbow-mode)
  (setq css-indent-offset 2)

  (defun config-mode-css()
    (rainbow-mode +1)
    (config-mode-prog)				; CSS mode does not inherit prog-mode
    )

  (add-hook 'css-mode-hook 'config-mode-css))

(provide 'config-css)
;;; config-css.el ends here


;; (use-package css-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.scss\\'" "\\.sass\\'")
;;   )

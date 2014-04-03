;; ----------------------------------------------------------------------------
;; Java Script Setup
;; ----------------------------------------------------------------------------
(provide 'js-setup)
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-bounce-indent-p t)		; Doesn't work well with <return>

(add-hook 'js-mode-hook		'my-javascript-mode-hook)
(add-hook 'js2-mode-hook	'my-javascript-mode-hook)
(defun my-javascript-mode-hook ()
  (interactive)
  ;; (flyspell-prog-mode)
  (hs-minor-mode t)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(return)]		'reindent-then-newline-and-indent
   [(M-return)]		'js2-line-break
   
   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "javascript "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "javascript "))

   ))


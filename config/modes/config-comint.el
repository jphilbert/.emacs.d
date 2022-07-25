
(use-package comint
  :commands comint-mode
  :config
  (define-keys		comint-mode-map
    [C-down]			'comint-next-prompt
    [C-up]			'comint-previous-prompt
    ;; These are nice (forget about previous/next-input)
    [down]			'comint-next-matching-input-from-input
    [up]				'comint-previous-matching-input-from-input
    [S-C-up]			'previous-line
    [S-C-down]			'next-line
    
    ;; ---------- Help ----------
    (kbd "C-h f")   	'man-at-point 
    [(f1)]			'(lambda ()
					   (interactive)
					   (google-query-at-point t "bash "))
    (kbd "C-h w")   	'(lambda ()
					   (interactive)
					   (google-query-at-point nil "bash "))

    ;; ---------- Frame Switching ----------
    [(f12)]			'switch-frame-previous
    [S-f12]              'shell-new)
  )
;; '(ansi-color-for-comint-mode t)
;; '(comint-move-point-for-output t)
;; '(comint-prompt-read-only nil)
;; '(comint-scroll-to-bottom-on-input t)
;; '(comint-scroll-to-bottom-on-output t)
;; '(comint-use-prompt-regexp t)

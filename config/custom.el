(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-buffer-alist
   '(("\\*Help.*\\*"
      (display-buffer-reuse-window display-buffer-pop-up-frame)
      (reusable-frames . 0)
      (cascade)
      (dedicated . 1)
      (window-parameters
       (mode-line-format . none))
      (pop-up-frame-parameters
       (unsplittable . t)
       (horizontal-scroll-bars)
       (vertical-scroll-bars)
       (width . 60)
       (top . 10)
       (left . 790)
       (minibuffer)
       (fit-frame-to-buffer-sizes 30 0 100 0)
       (fit-frame-always . t)))))
 '(display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-pop-up-frame)
     (reusable-frames . 0)
     (cascade . t)))
 '(package-selected-packages
   '(flyspell-correct-popup flyspell-correct corfu transient rg zenburn-theme yasnippet which-key vertico swiper smartrep smartparens smart-hungry-delete rainbow-mode rainbow-delimiters projectile powerline orderless operate-on-number markdown-mode marginalia hl-todo f expand-region ess embark crux consult auto-complete ac-js2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:height 100 :slant normal :weight normal))))
 '(ac-selection-face ((t (:height 100 :slant normal :weight normal))))
 '(ac-yasnippet-candidate-face ((t (:inherit 'ac-candidate-face :foreground "#6C3333"))))
 '(ac-yasnippet-selection-face ((t (:inherit 'ac-selection-face :foreground "#D0BF8F"))))
 '(completions-annotations ((t (:foreground "#6C3333"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#6F8F6F"))))
 '(font-lock-comment-face ((t (:foreground "#6F8F6F"))))
 '(font-lock-number-face ((t (:foreground "#5C888B"))) t)
 '(font-lock-relation-operator-face ((t (:foreground "#DFAF8F" :weight bold))) t)
 '(lazy-highlight ((t (:foreground "#C77138" :weight bold :background "#494949"))))
 '(mode-line ((t (:foreground "#8FB28F" :background "#000000" :height 80 :box (:line-width -1 :style released-button)))))
 '(mode-line-1 ((t (:inherit mode-line :background "#2B2B2B"))))
 '(mode-line-1-inactive ((t (:inherit mode-line-inactive :background "#3F3F3F"))))
 '(mode-line-2 ((t (:inherit mode-line :background "#3F3F3F"))))
 '(mode-line-2-inactive ((t (:inherit mode-line-inactive :background "#4F4F4F"))))
 '(mode-line-column-warn-face ((t (:inherit mode-line-position-face :inverse-video t :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "#AFD8AF" :background "#2B2B2B"))))
 '(mode-line-mode-face ((t (:inherit mode-line-2 :foreground "#94BFF3" :background nil :weight bold))))
 '(mode-line-mode-inactive-face ((t (:inherit mode-line-2-inactive :foreground "#8CD0D3"))))
 '(mode-line-modified-face ((t (:inherit mode-line :foreground "#CC9393" :background nil :weight bold :box (:line-width 2 :color "#CC9393")))))
 '(mode-line-process-face ((t (:inherit mode-line-2 :foreground "#F0DFAF" :background nil :weight bold))))
 '(mode-line-process-inactive-face ((t (:inherit mode-line-2-inactive :foreground "#E0CF9F"))))
 '(mode-line-read-only-face ((t (:foreground "#AC7373"))))
 '(vertico-current ((t (:foreground "#DCDCCC" :weight bold :background "#4C7073" :underline nil)))))

;; ----------------------------------------------------------------------------
;; Web Mode Setup
;; ----------------------------------------------------------------------------
(provide 'web-setup)


(add-to-list
 'browse-url-filename-alist
 '("C:/Users/JPHil_000/Documents/WWW/" . "http://localhost/"))


(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions
      '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode t)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'html-mode-hook		'my-html-mode-hook)
(defun my-html-mode-hook ()
  (interactive)
  (flyspell-prog-mode)

  ;; (sgml-mode)

  (auto-indent-minor-mode -1)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(return)]		'newline

   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   (kbd "C-h w")   	'(google-query-mode-at-point-lucky)
   (kbd "C-h W")   	'(google-query-mode-at-point)

   (kbd ">")		'(lambda ()
			   (interactive)
			   (insert ">")
			   (save-excursion
			     (sgml-close-tag))
			   )
   (kbd "C-<")		'tag-word-or-region
   ))

(add-hook 'js-mode-hook		'my-javascript-mode-hook)
(defun my-javascript-mode-hook ()
  (interactive)
  (flyspell-prog-mode)
  (auto-indent-minor-mode 1)
  (hs-minor-mode t)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(return)]		'newline

   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   (kbd "C-h w")   	'(google-query-mode-at-point-lucky)
   (kbd "C-h W")   	'(google-query-mode-at-point)))

(add-hook 'css-mode-hook		'my-css-mode-hook)
(defun my-css-mode-hook ()
  (interactive)
  (flyspell-prog-mode)
  (auto-indent-minor-mode 1)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(return)]		'newline

   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   (kbd "C-h w")   	'(google-query-mode-at-point-lucky)
   (kbd "C-h W")   	'(google-query-mode-at-point)))

;; --------------------------------------------------------------------------
;; Additional Functions
;; --------------------------------------------------------------------------
(defun tag-word-or-region (tag)
  "Surround current word or region with a given tag."
;; http://www.johndcook.com/blog/2010/08/05/emacs-command-to-add-html-tags/
    (interactive "sEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" tag ">"))
        (if (and transient-mark-mode mark-active)
            (progn
	      (setq pos2 (region-end)
		    pos1 (region-beginning))
                (goto-char pos2)
                (insert end-tag)
                (goto-char pos1)
                (insert start-tag)
		(goto-char (+ pos2 (string-width start-tag)
			      (string-width end-tag))))
	  (progn
	    (message "bye")
                (setq bds (bounds-of-thing-at-point 'symbol))
                (goto-char (cdr bds))
                (insert end-tag)
                 (goto-char (car bds))
                 (insert start-tag)
		 (goto-char (+ (cdr bds)
			       (string-width start-tag)
			       (string-width end-tag)))))))
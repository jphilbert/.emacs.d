;; ----------------------------------------------------------------------------
;; Web Mode Setup
;; ----------------------------------------------------------------------------
(provide 'web-setup)
(require 'web-mode)
(require 'js-setup)

(setq-default web-mode-enable-current-element-highlight t)

(add-to-list				; Home
 'browse-url-filename-alist
 '("C:/Users/JPHil_000/Documents/WWW/" . "http://localhost/"))

(add-to-list				; Work
 'browse-url-filename-alist
 '("C:/Users/hilbertjp/Local_Files/WWW" . "http://localhost/"))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'html-mode-hook		'my-html-mode-hook)
(defun my-html-mode-hook ()
  (interactive)
  (flyspell-prog-mode)
  (web-mode)

  (add-to-list 'ac-sources 'ac-source-css-property)
  (auto-complete-mode)
  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(f3)]               'web-mode-fold-or-unfold
   
   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Movement ----------
   (kbd   "<M-left>") 		'web-mode-element-previous
   (kbd   "<M-right>")		'web-mode-element-next

   (kbd   "<M-down>")		'(lambda ()
				   (interactive)
				   (web-mode-element-end)
				   (web-mode-element-next))
   (kbd   "<M-up>")		'(lambda ()
				   (interactive)
				   (web-mode-tag-previous)
				   (web-mode-tag-match))


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "html "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "html "))
   (kbd "C-h e")   	'web-mode-errors-show
   ))


(add-hook 'css-mode-hook		'my-css-mode-hook)
(defun my-css-mode-hook ()
  (interactive)
  (flyspell-prog-mode)
  (hs-minor-mode)
  ;; (auto-indent-minor-mode 1)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "CCS "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "CSS "))
   ))

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


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  
  :hook
  (markdown-mode-hook
   . (lambda ()
	  (auto-complete-mode)
	  (auto-fill-mode 0)
	  (setq
	   tab-width				2
	   fill-column				99999999
	   electric-pair-pairs		'((?`. ?`)))

	  (define-keys
	    markdown-mode-map
	    ;; ---------- Evaluation ----------
	    [(shift return)]		markdown-preview

	    (kbd "<tab>")			tab-to-tab-stop
	    ;; ---------- Styles ----------
	    (kbd "C-i")	          markdown-insert-italic
	    (kbd "C-b")	          markdown-insert-bold 
	    (kbd "C-l")	          markdown-insert-link 
	    (kbd "C-j")	          markdown-insert-code 
	    
	    ;; [(f3)]				markdown-shifttab
	    
	    ;; ---------- Hide-Show ----------
	    [(f3)]				markdown-hide-subtree
	    [(shift f3)]			markdown-show-subtree
	    
	    ;; ---------- Help ----------
	    "\C-hf"		(lambda ()
					  (interactive)
					  (browse-url
					   (concat "https://github.com/adam-p/markdown-here"
							 "/wiki/Markdown-Cheatsheet")))
	    [(S-f1)]	     (lambda ()
					  (interactive)
					  (browse-url
					   (concat "https://github.com/adam-p/markdown-here"
							 "/wiki/Markdown-Cheatsheet")))
	    (kbd "C-h w")	(lambda ()
					  (interactive)
					  (google-query-at-point nil "markdown ")))
	  ))

  :custom
  (markdown-command			"pandoc")

  :config
  (message ">>> markdown-mode: config run")
  (message "%s" markdown-command)

  )

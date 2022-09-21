;; ------------------------------------------------------------------------- ;;
;; Web Searching Commands
;; - see also webjump.el
;; ------------------------------------------------------------------------- ;;
(defun thesaurus-search (word)
  "Automatically queries thesaurus.com for word at point."
  (interactive "MThesaurus: ")
  (let ((url "https://www.thesaurus.com/browse/"))
    (browse-url (format "%s%s" url (url-hexify-string word)))))

(defun thesaurus-at-point ()
  "Automatically queries thesaurus.com for word at point."
  (interactive)
  (let (pt_0 pt_1)
    (if (use-region-p)
        (progn
          (setq pt_0 (region-beginning) 
                pt_1 (region-end))
          (deactivate-mark))
      (save-mark-and-excursion
	    (skip-syntax-forward "w")
        (setq pt_1 (point))
        (skip-syntax-backward "w")
        (setq pt_0 (point))))
    
    (when (/= pt_0 pt_1)
      (thesaurus-search (buffer-substring-no-properties pt_0 pt_1)))))


(defvar web-search-url          "http://www.google.com/search?q="
  "URL used to query the web.")

(defvar web-search-url-direct   "https://google.com/search?btnI=1&q="
  "URL used to query the web and directly follow first result. ")

(defvar-local google-mode-prefix nil
  "Prefix to append to google queries at point in a buffer. If
  nil the functions will attempt to use the `major-mode' of the
  buffer. ")

(setq web-search-url            "https://duckduckgo.com/?q="
      web-search-url-direct     "https://duckduckgo.com/?q=\\+")

(defun google-search (term &optional results)  
  (interactive "MSearch Term: ")
  (let* ((url (if results web-search-url web-search-url-direct))
	     (term (replace-regexp-in-string
		         "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" term))
	     (term (replace-regexp-in-string "(.*)" "" term)))
    (browse-url (format "%s%s" url (url-hexify-string term)))))

(defun google-at-point (&optional results prefix)
  "Automatically queries Google for object at point.
Additionally can use 'feeling first-result' and append a prefix (useful
for major programming modes to filter results)"
  (interactive)
  ;; Set google-mode-prefix unless already set
  (unless google-mode-prefix
    (setq google-mode-prefix
          (replace-regexp-in-string
	       "-.*" ""
	       (symbol-name (with-current-buffer (current-buffer) major-mode)))))
  
  (let ((prefix (or prefix google-mode-prefix)) ; use prefix if given
        (symbol-regexp "\\(\\s_\\|\\sw\\|\\.\\)+")
        pt_0 pt_1 term)
    
    (if (use-region-p)
        (progn
          (setq pt_0 (region-beginning) 
                pt_1 (region-end))
          (deactivate-mark))
      (save-mark-and-excursion
        (skip-syntax-backward "_w.")
        (setq pt_0 (point))
        (when (looking-at symbol-regexp)
          (goto-char (match-end 0)))
        (when (looking-at "(")
          (forward-list))
        (setq pt_1 (point))))
    
    (when (/= pt_0 pt_1)
      (setq term (buffer-substring-no-properties pt_0 pt_1))
      (when (length prefix)
        (setq term (concat prefix " " term)))
      (google-search term results))))

(defun google-at-point-show-results ()
  (interactive)  
  (google-at-point t))



(provide 'web-search)
;;; WEB-SEARCH.EL ends here 

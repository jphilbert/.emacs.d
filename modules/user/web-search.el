;; -------------------------------------------------------------------------- ;;
;; Web Searching Commands                                                     ;;
;; - see also webjump.el                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defvar web-search-url "https://google.com/search?q="
  "URL used to query the web.

Some options are:
    - https://google.com/search?q=
    - https://duckduckgo.com/?q=")

(defvar web-search-url-direct "https://google.com/search?btnI=1&q="
  "URL used to query the web and directly follow first result.

Some options are:
    - https://google.com/search?btnI=1&q=
    - https://duckduckgo.com/?q=\\+")

(defvar-local web-search-mode-prefix nil
  "Prefix to append to web-search queries.

A string (or function returning a string) which will be added to
`web-search-at-point' searches. If nil, the `major-mode' of the
buffer (without `-mode') will be used.")

(defun web-search (term &optional results)  
  (interactive "MSearch Term: ")
  (let* ((url (if results web-search-url web-search-url-direct))
	     (term (replace-regexp-in-string
		         "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" term))
	     (term (replace-regexp-in-string "(.*)" "" term)))
    (browse-url (format "%s%s" url (url-hexify-string term)))))

(defun web-search-at-point (&optional results prefix)
  "Automatically queries web for object at point.

Additionally can direct to first result (RESULTS is non-nil) or
append a PREFIX (useful for programming modes to filter results)"
  
  (interactive)
  ;; Set web-search-mode-prefix unless already set
  
  
  (let* ((prefix (or prefix web-search-mode-prefix)) ; use prefix if given
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
      (unless prefix
        (setq prefix
              (replace-regexp-in-string
               "-+" " " (symbol-name (buffer-major-mode))))
        (setq prefix
              (replace-regexp-in-string
               " *mode *" "" prefix)))

      (when (functionp prefix)
        (setq prefix (funcall prefix)))
        
      (when (length prefix)
        (setq term (concat prefix " " term)))
      
      (web-search term results))))

(defun web-search-at-point-direct ()
   "Queries web for object at point and directs to first result"
  (interactive)  
  (web-search-at-point t))



;; -------------------------------------------------------------------------- ;;
;; Thesaurus on the Web                                                       ;;
;; -------------------------------------------------------------------------- ;;
(defvar thesaurus-url "https://www.thesaurus.com/browse/"
  "URL used to query thesaurus on the web.

Some options are:
    - https://www.thesaurus.com/browse/")

(defun thesaurus-search (word)
  "Queries thesaurus for a word on the web."
  (interactive "MThesaurus: ")
  (browse-url
   (format "%s%s"
           thesaurus-url
           (url-hexify-string word))))

(defun thesaurus-at-point ()
  "Automatically queries thesaurus for word at point on the web."
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




(provide 'web-search)
;;; WEB-SEARCH.EL ends here 

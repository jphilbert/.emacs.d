(provide 'misc)

(defun today ()
  "Insert string for today's date nicely formatted"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(defun google-query-at-point (&optional lucky prefix)
  "Automatically queries Google for object (using expand-region)
at point. Additionally can use 'feeling lucky' and append a
prefix (useful for major programming modes to filter results)"
  (interactive)
  (let ((url
	 (if lucky
	     "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q="
	   "http://www.google.com/search?q="))
	(encoding 'utf-8)
	(minibuffer-string "Google: ")
	query)
    (save-excursion
      (unless (region-active-p)
	(er/mark-method-call))
      (setq query (websearch-trim
		   (buffer-substring-no-properties
		    (region-beginning) (region-end)))))
    (setq query (replace-regexp-in-string "(.*)" "" query))
    (setq query (concat prefix query))
    (funcall websearch-browse-url-function
	     (format "%s%s" url (websearch-url-encode query encoding)))
    ))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is
selected and current line is not blank and we are not at the end
of the line, then comment current line. Replaces default behavior
of comment-dwim, when it inserts comment at the end of the line."
  ;; Original idea from
  ;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
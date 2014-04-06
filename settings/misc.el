
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

(defun google-query-mode-at-point-lucky ()
  "Automatically queries Google for object (using expand-region) at
point. Additionally appends the current buffer's mode to the search.  This is
the lucky (immediate) version."
  (interactive)
  (let (m)
    (setq m (replace-regexp-in-string
	     "-.*" ""
	     (symbol-name (with-current-buffer
			      (current-buffer)
			    major-mode))))
    (message (concat m " "))
    (google-query-at-point t (concat m " "))))

(defun google-query-mode-at-point ()
  "Automatically queries Google for object (using expand-region) at
point. Additionally appends the current buffer's mode to the search."
  (interactive)
  (let (m)
    (setq m (replace-regexp-in-string
	     "-.*" ""
	     (symbol-name (with-current-buffer
			      (current-buffer)
			    major-mode))))
    (message (concat m " "))
    (google-query-at-point nil (concat m " "))))

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

(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun copy-code-snippet (begin end)
  ;; http://stackoverflow.com/a/3519790
  (interactive "r")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      ;; (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun next-non-blank-line ()
  "Skips to the next non-blank line"
  (interactive)
  (next-line)  
  (while (and (not (eobp)) (empty-line-p))
    (next-line)))

(defun empty-line-p ()
  "Returns t if cursor is at an empty line "
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t\f\r]*$")))

(defun cua-remove-blanks-in-rectangle ()
  "Removes blanks in each line of CUA rectangle."
  (interactive)
  (if buffer-read-only
      (message "Cannot replace in read-only buffer")
    (cua--rectangle-operation 'keep nil t 1 nil
			      '(lambda (s e l r)
				 (if (re-search-forward "\\s-+" e t)
				     (replace-match "" nil nil))))))

(defun cua-replace-blanks-in-rectangle (newtext)
  "Replace blanks with NEWTEXT in each line of CUA rectangle."
  (interactive "sNew text: ")
  (if buffer-read-only
      (message "Cannot replace in read-only buffer")
    (cua--rectangle-operation 'keep nil t 1 nil
			      '(lambda (s e l r)
				 (if (re-search-forward "\\s-+" e t)
				     (replace-match newtext nil nil))))))

(defun explorer (dir)
  "Launch the windows explorer in the current directory and selects current file"
  ;;; Windows explorer to open current file - Arun Ravindran
  ;; Altered slightly to be interactive - JPH
  (interactive "fDirectory / File to Open: ")
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/select,"
   	   (convert-standard-filename (expand-file-name dir))))
   ;; (concat "/select,"
   ;; 	   (convert-standard-filename buffer-file-name)))
  )

(defun apply-function-to-region-lines (fn)
  "Apply function FN to each line in a region"
  (if (and transient-mark-mode mark-active)
      (save-excursion
	(goto-char (region-end))
	(let ((end-marker (copy-marker (point-marker)))
	      next-line-marker)
	  (goto-char (region-beginning))
	  (if (not (bolp))
	      (forward-line 1))
	  (setq next-line-marker (point-marker))
	  (while (< next-line-marker end-marker)
	    (let ((start nil)
		  (end nil))
	      (goto-char next-line-marker)
	      (save-excursion
		(setq start (point))
		(forward-line 1)
		(set-marker next-line-marker (point))
		(setq end (point)))
	      (save-excursion
		(let ((mark-active nil))
		  (narrow-to-region start end)
		  (funcall fn)
		  (widen)))))
	  (set-marker end-marker nil)
	  (set-marker next-line-marker nil)))
    (funcall fn)))
  
(defun tab-to-tab-stop-magic ()
  "Insert tabs to line or region"
  (interactive)
  (apply-function-to-region-lines 'tab-to-tab-stop))

(defun ac-show-quick-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
	     (or (symbol-at-point)
		 (progn (backward-up-list)
			(forward-char)
			(symbol-at-point))))))
    (pos-tip-show (ac-symbol-documentation s)
		  'popup-tip-face
		  ;; 'alt-tooltip
		  (point)
		  nil
		  -1)))

(provide 'misc)

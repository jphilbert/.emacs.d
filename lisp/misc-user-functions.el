(provide 'misc-user-functions)

(defun today ()
  "Insert string for today's date nicely formatted"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))


;; ------------------------------------------------------------------------- ;;
;; File / Buffer Functions
;; ------------------------------------------------------------------------- ;;
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
  )

(defun get-scratch-buffer ()
  "Displays the scratch buffer if available, otherwise creates a new one."
  (interactive)
  (let ((scratch-buffer (get-buffer "*scratch*")))
    (unless scratch-buffer
	 (setq scratch-buffer
		  (set-buffer (get-buffer-create "*scratch*")))
	 (insert initial-scratch-message)
	 (emacs-lisp-mode))
    (unless (eq (current-buffer) scratch-buffer)
	 (display-buffer-other-frame scratch-buffer))))

(defun rename-current-buffer-file ()
  "Renames current buffer AND file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	    (filename (buffer-file-name))
	    (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name
				   "New name: "
				   (file-name-directory filename)
				   basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun mode-line-toggle ()
  (interactive)
  (if mode-line-format
	 (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))))

(defun kill-buffer-smart ()
  (interactive)
  (cond
   ;; 1 frame + 1 window --> save buffer + kill emacs
   ((= (length (frame-list)) 1)
    (kill-emacs))
   
   ;; N frame
   ;;		+ 1 window 
   ((one-window-p)
    ;;				+ 1 buffer-window --> kill buffer (kill frame)
    (if (= (length (get-buffer-window-list nil nil t)) 1)
	   (kill-buffer)
	 ;;				+ N buffer-window --> kill frame
	 (kill-frame)))
   
   ;;		 + N windows
   ;;				 + active buffer in N windows --> kill window
   ((> (length (get-buffer-window-list nil nil t)) 1)
    (delete-window))
   ;; ;;				 + active buffer not in upper left --> kill window
   ;; ((equal (frame-first-window) (selected-window))
   ;;  (delete-window))
   
   ;; OTHERWISE --> kill other windows
   (t
    (delete-other-windows))
   )
  )


;; ------------------------------------------------------------------------- ;;
;; Web Searching
;; - require expand-region and websearch
;; ------------------------------------------------------------------------- ;;
(defun thesaurus-at-point ()
  "Automatically queries thesaurus.com (via `websearch') for word (using
`expand-region') at point."
  (interactive)
  (let ((url "https://www.thesaurus.com/browse/")
	(encoding 'utf-8)
	(minibuffer-string "Thesaurus: ")
	query)
    (save-excursion
      (unless (region-active-p)
	(er/mark-word))
      (setq query (buffer-substring-no-properties
			    (region-beginning) (region-end)))) 
    (funcall websearch-browse-url-function
		   (format "%s%s" url (websearch-url-encode query encoding)))
    ))

(defun google-query-at-point (&optional lucky prefix)
  "Automatically queries Google (via `websearch') for object (using
`expand-region') at point. Additionally can use 'feeling lucky' and append a
prefix (useful for major programming modes to filter results)"
  (interactive)
  (let ((url
	 (if lucky
	     "https://google.com/search?btnI=1&q="
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
  "Automatically queries Google (via `websearch') for object (using `expand-region') at point. Additionally appends the current buffer's mode to the search.  This is the lucky (immediate) version."
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
  "Automatically queries Google for object (using `expand-region') at
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


;; ------------------------------------------------------------------------- ;;
;; Navigation
;; ------------------------------------------------------------------------- ;;
(defun next-non-blank-line ()
  "Skips to the next non-blank line"
  (interactive)
  (next-line)  
  (while (and (not (eobp)) (empty-line-p))
    (next-line)))

(defun previous-non-blank-line ()
  "Skips to the previous non-blank line"
  (interactive)
  (previous-line)  
  (while (and (not (bobp)) (empty-line-p))
    (previous-line)))


;; ------------------------------------------------------------------------- ;;
;; Modification
;; ------------------------------------------------------------------------- ;;
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

(defun tab-to-tab-stop-magic ()
  "Insert tabs to line or region"
  (interactive)
  (apply-function-to-region-lines 'tab-to-tab-stop))

(defun kill-empty-lines ()
  "Kills all empty lines. If mark is active, operates on the region. Otherwise,
operate from point to the end of (the accessible portion of) the buffer"
  (interactive)
  (flush-lines "^[[:space:]]*$" nil nil t))

(defun yank-pop-forwards (arg)
  "Opposite of `yank-pop' in that it will cycle forward to more recent yanks"
      (interactive "p")
      (yank-pop (- arg)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	   ;; This would override `fill-column' if it's an integer.
	   (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


;; --------------- ;;
;;  CUA Rectangle  ;;
;; --------------- ;;
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


;; ------------------------------------------------------------------------- ;;
;; Utility / Non-interactive Functions
;; ------------------------------------------------------------------------- ;;
(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun empty-line-p ()
  "Returns t if cursor is at an empty line "
  (save-excursion
    (beginning-of-line)
    (looking-at "^[[:space:]]*$")))

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


;; ------------- ;;
;;  Key Binding  ;;
;; ------------- ;;
(defun global-set-keys (&rest key-command)
  "Defines a series of KEY-COMMAND globally similar to `global-set-key'.  It
is assumed that the KEY-COMMAND come in pairs of KEY and COMMAND. If COMMAND is nil the key is unset.

See also `local-set-keys' and `define-keys'"
  (while key-command
    (let ((k (pop key-command))
          (f (pop key-command)))
      
      (if f
		(global-set-key k f)
	   (global-unset-key k)))))

(defun local-set-keys (&rest key-command)
  "Defines a series of KEY-COMMAND locally similar to `local-set-key'.  It
is assumed that the KEY-COMMAND come in pairs of KEY and COMMAND. If COMMAND is nil the key is unset.

See also `global-set-keys' and `define-keys'"
  (while key-command
    (let ((k (pop key-command))
          (f (pop key-command)))
      
      (if f
		(local-set-key k f)
	   (local-unset-key k)))))

(defun define-keys (keymap &rest key-def)
  "Defines a series of KEY-DEF for a KEYMAP similar to `define-key'.  It
is assumed that the KEY-DEF are grouped in pairs of KEY and DEFINITION.

See also `global-set-keys' and `local-set-keys'"
  (while key-def
    (let ((k (pop key-def))
          (f (pop key-def)))
	 (define-key keymap k f))))

(define-obsolete-function-alias
  'global-set-many-keys #'global-set-keys "2021-8-19")
(define-obsolete-function-alias
  'local-set-many-keys #'local-set-keys "2021-8-19")
(define-obsolete-function-alias
  'define-many-keys #'define-keys "2021-8-19")



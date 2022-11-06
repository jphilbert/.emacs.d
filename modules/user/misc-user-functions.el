(require 's)

(defun s-format-context (format-str)
  (eval (s-lex-fmt|expand format-str)))

(defun message-format (format-string &rest args)
  (message (s-format-context format-string) args))

(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  ;; (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

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

(defun get-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named `untitled' or `untitled<2>', `untitled<3>', etc.

It returns the buffer (for elisp programming).

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer buffer)    
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    (display-buffer buffer)
    buffer))

(defun mode-line-toggle ()
  (interactive)
  (if mode-line-format
	  (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))))

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
;; TODO replace with `f-read'
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

(provide 'misc-user-functions)

;; ------------------------------------------------------------------------- ;;
;; Package Updating
;; ------------------------------------------------------------------------- ;;
(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun prelude-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory prelude-dir 0))

(defun prelude-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))



(defun end-of-buffer-all ()
  "Moves the cursor to the end for all windows showing current buffer."
  (interactive)
  (goto-char (point-max))
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (while windows
      (set-window-point (car windows) (point-max))
      (setq windows (cdr windows)))))

;; DEPRECATED
;; see SAVE-SELECTED-WINDOW, WITH-SELECTED-WINDOW, and WITH-SELECTED-FRAME
;; (defmacro save-frame-excursion (&rest x)
;;   "Like save-window-excursion, however restores current frame"
;;   (let (this-frame (selected-frame))
;;     (save-window-excursion x)
;;     (raise-frame this-frame)))






;;; Code:

;; ;; Teach Emacs how to interpret various modifier keys
;; (setq w32-pass-lwindow-to-system nil)
;; (setq w32-lwindow-modifier 'super) ; Left Windows key

;; (setq w32-pass-rwindow-to-system nil)
;; (setq w32-rwindow-modifier 'super) ; Right Windows key

;; (setq w32-pass-apps-to-system nil)
;; (setq w32-apps-modifier 'hyper) ; Menu/App key

;; ;; Git setup (assuming you've installed Git for Windows)
;; (when (file-exists-p "C:/Program Files/Git/bin")
;;   (add-to-list 'exec-path "C:/Program Files/Git/bin")
;;   (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
;;   (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH"))))

;; ;; needed for arc-mode (it allows you to open archives in Emacs)
;; (if (file-exists-p "C:/Program Files/7-Zip")
;;     (add-to-list 'exec-path "C:/Program Files/7-Zip")
;;   (message "7-Zip not found. It's a good idea to install it."))


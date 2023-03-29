(require 's)
(require 'f)

(defun today ()
  "Insert string for today's date nicely formatted"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))


(defun advice-remove-all (func)
  "Remove all advices from symbol FUNC."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove func advice)) func))

(defun browse-file-as-chrome-app (filename &optional position size user-data)
  "Opens FILENAME in Chrome in application mode.

Opens file in Chrome using --app=FILENAME switch. Optionally, the
SIZE and POSITION can be specified for the window as a coordinate
list (x y). If either are given, --user-data-dir will be set
to variable `temporary-file-directory' unless explicitly set." 
  (if size
      (-let (((x y) size))
        (setq size (s-format-context " --window-size=${y},${x}"))
        (setq user-data (or user-data temporary-file-directory)))
    (setq size ""))
  
  (if position
      (-let (((x y) position))
        (setq position (s-format-context " --window-position=${x},${y}"))
        (setq user-data (or user-data temporary-file-directory)))
    (setq position ""))

  (if user-data
      (setq user-data (s-format-context " --user-data-dir=${user-data}"))
    (setq user-data ""))

  (w32-shell-execute
   "open"
   "chrome.exe"
   (s-format-context "--app=${filename}${size}${position}${user-data}")))


;; -------------------------------------------------------------------------- ;;
;; String / Messages Functions                                                ;;
;; -------------------------------------------------------------------------- ;;
(defun s-format-context (format-str)
  "`s-format' with the current environment.

FORMAT-STR may use the `s-format' variable reference to refer to
any variable:

 (let ((x 1))
   (s-lex-format \"x is: ${x}\"))

The values of the variables are interpolated with \"%s\" unless
the variable `s-lex-value-as-lisp' is `t' and then they are
interpolated with \"%S\"."
  (eval (s-lex-fmt|expand format-str)))

(defun message-format (format-string &rest args)
  "Display a message like `message'.

The first argument is a format control string, and the rest are data
to be formatted under control of the string.  In addition to percent
sign (%) to denote optional arguments, FORMAT-STR may use the
`s-format' variable reference to refer to any variable.

Example:

 (let ((x 1))
   (message-format \"x is: ${x} and arg is: %s\" \"abc\"))

> \"x is: 1 and arg is: (abc)\"
"
  (message (s-format-context format-string) args))

(defun message-symbol (&rest symbols)
  (dolist (s symbols)
    (setq s (symbol-name s))
    (message-format (s-concat s ": ${" s "}"))))


;; ------------------------------------------------------------------------- ;;
;; File / Buffer Functions                                                   ;;
;; ------------------------------------------------------------------------- ;;
(defun generate-new-buffer-name+ (name)
  "Generate a new buffer name based off of NAME.

Returns the next available buffer name in the sequence based on
NAME.  NAME is assumed to be of the format _*STRING_[%s]*_, where
STRING is the base name and %s is the numeric placeholder.  The
brackets can be any pairing (), [], {}, <>, or omitted.
Additionally the spaces (_) and stars (*) are optional.

If the placeholder %s is omitted it will be assumed NAME =
NAME_%s. For example, if '*Scratch*' is given, the format would
assumed to be '*Scratch %s*' thus the sequence of possible names
would be '*Scratch*', '*Scratch 2*', '*Scratch 3*', etc.
 
See also `generate-new-buffer-name'. "
  (if-let
      (;; ---------- Parse NAME ---------- ;;
       ;; 1) Strip off white space and * if exist
       (buffer0
        (or
         (cadr (s-match "^[[:space:]]*\\*\\(.*\\)\\*[[:space:]]*$"
                        name))
         name))
       ;; 2) If wrapped in *, store
       (outer-mask
        (s-replace buffer0 "%s" name))
       ;; 3) Identify spacing and wrapping for number
       (numeric-suffix
        (or
         (car
          (s-match "[[:space:]]*[\[(<{)]?%s[\]>})]?$"
                   buffer0))
         " %s"))
       ;; 4) Trim off numeric part to get the bare buffer prefix
       (buffer0
        (s-replace numeric-suffix "" buffer0))

       ;; 5) Recombine to get format of subsequent and initial buffers
       (buffer+ (format outer-mask (s-concat buffer0 numeric-suffix)))
       (buffer0 (format outer-mask buffer0))
       
       ;; ---------- Calculate Next Buffer in Sequence ---------- ;;
       ;; If the bare buffer (initial in sequence) does not exists, break and
       ;; return it (BUFFER0)
       ((get-buffer buffer0))
       (buffer2 (format buffer+ 2))
       
       ;; Get the list of incremented buffers
       (buffer-numbers
        (->> (buffer-list)
             (--map                     ; loop through buffer list
              (s-match
               (format (s-concat "^" (regexp-quote buffer+) "$")
                       "\\(\[0-9\]+\\)")
               (buffer-name it)))
             (-non-nil)                 ; remove unmatched
             (-map #'-second-item)      ; get subgroup
             (-map #'string-to-number))) ; convert to integer

       ;; Get the contiguous list of buffers 
       (full-list
        (-iterate #'1+ 2 (max 1 (-max buffer-numbers))))
       
       ;; The next available is the smallest value not in buffer list
       (next-inc
        (-min (-difference full-list buffer-numbers))))

      (format buffer+ next-inc)         ; THEN
    (or buffer2 buffer0)                ; ELSE
    ))

(defun generate-new-buffer+ (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name+'."
  (get-buffer-create (generate-new-buffer-name+ name)))


(defun windows-explore (file-or-directory)
  "Launch windows explorer in directory of FILE-OR-DIRECTORY"
  ;;; Windows explorer to open current file - Arun Ravindran
  ;;; Altered slightly to be interactive - JPH
  (interactive "fDirectory / File to Open in Explorer: ")
  (message "%s" (concat "/select,"
           (replace-regexp-in-string
            "/" "\\"
            (f-canonical file-or-directory) t t)))
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/select,"
           (replace-regexp-in-string
            "/" "\\"
            (f-canonical file-or-directory) t t)))
  )

(defun windows-explore-this ()
  "Launch windows explorer in this file's directory"
  (interactive)
  (windows-explore (f-this-file)))

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
  (let ((buffer (generate-new-buffer+ "untitled (%s)")))
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

(defun next-paragraph ()
  "Move to start of the next paragraph.

Move point to the character that starts the next paragraph.
Unlike the function `forward-paragraph', this excludes newline
and tabs.  Functions more like the function
`start-of-paragraph-text'."
  (interactive)
  (forward-paragraph)
  (skip-chars-forward " \t\n"))

(defun back-paragraph ()
  "Move to start of current or previous paragraph.

Move point to the character that starts the current paragraph
unless point is at the start, in which case it move to the start
of the previous paragraph.  Unlike the function
`backward-paragraph', this excludes newline and tabs.  Functions
more like the function `start-of-paragraph-text'."
  (interactive)
  (if (eq (preceding-char) ?\n) (forward-char -1))
  (forward-paragraph -1)
  (skip-chars-forward " \t\n"))


;; -------------------------------------------------------------------------- ;;
;; Modification                                                               ;;
;; -------------------------------------------------------------------------- ;;
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


;; -------------------------------------------------------------------------- ;;
;; Utility / Non-interactive Functions                                        ;;
;; -------------------------------------------------------------------------- ;;
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

(defun buffer-major-mode (&optional buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer (or buffer-or-name (current-buffer)) major-mode))



(provide 'misc-user-functions)
;;; misc-user-functions.el ends here



;; -------------------------------------------------------------------------- ;;
;; Package Updating                                                           ;;
;; -------------------------------------------------------------------------- ;;
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


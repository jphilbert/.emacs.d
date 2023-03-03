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

(defun comment-horizontal-line (&optional width)
  "Insert horizontal line from current column.

If the line is empty, a string of dashes wrapped by comment
characters will be inserted between `current-column' to WIDTH or
`fill-column', which ever is smaller. For example, in lisp-mode
yields:

    ;; ----------... ...---------- ;;

If the line is not empty, nothing occurs.
"  
  (if (not (empty-line-p))
      (message "line is not empty")
    
    (let*
        ((width (or width fill-column))
         (width (min width (- fill-column (current-column))))
         (line-start (comment-padright comment-start comment-add))
         (line-end (comment-padleft comment-start comment-add))
         (line-middle (s-repeat (- width
                                   (length line-start)
                                   (length line-end))
                                "-")))
      (insert (s-concat line-start line-middle line-end))
      (delete-horizontal-space)
      (forward-char))))

(defun comment-horizontal-line-full ()
  "Insert horizontal line from the mode indentation to `fill-column'"
  (interactive)
  (indent-according-to-mode)
  (comment-horizontal-line))

(defun comment-header-line-fixed ()
  "Pads line with a set amount of dashes before commenting."
  
  (interactive)
  (uncomment-region (point-at-bol) (point-at-eol))
  (beginning-of-line)
  (delete-horizontal-space)
  (end-of-line)
  (delete-horizontal-space)
  (indent-according-to-mode)
  
  (let*
      ((line-middle     (s-repeat 10 "-"))
       (line-start      (s-concat
                         (comment-padright comment-start comment-add)
                         line-middle " "))
       (line-end        (s-concat
                         " " line-middle
                         (comment-padleft comment-start comment-add))))

    (if (<= fill-column
            (+ (length line-start) (length line-end) (current-column)))
        (comment-region (point) (point-at-bol))
      (back-to-indentation)
      (insert line-start)
      (end-of-line)
      (insert line-end)
      (backward-char (length line-end)))))

(defun comment-header-line-full ()
  "Centers and fully pads line with dashes before commenting."

  (interactive)
  (uncomment-region (point-at-bol) (point-at-eol))
  (beginning-of-line)
  (delete-horizontal-space)
  (end-of-line)
  (delete-horizontal-space)  
  (indent-according-to-mode)
  ;; This leaves the cursor at the end of the line of length: indent + text
  
  (let*
      ((line-start (comment-padright comment-start comment-add))
       (line-end (comment-padleft comment-start comment-add))
       ;; comment + pad + space + text + space + pad + comment
       ;;  = 2 * (comment + pad + space) + text
       ;;  = fill - indent
       ;;  --> 2 * pad = fill - text - indent - 2 * space - 2 * comment
       (padding-total (- fill-column
                           (length line-start)
                           (length line-end)
                           (current-column)
                           2))
       (padding-left (/ padding-total 2))
       (padding-right (+ padding-left (% padding-total 2))))

    (back-to-indentation)
    (if (<= padding-total 0)
        (comment-region (point) (point-at-eol))
      (insert (s-concat line-start (s-repeat padding-left "-") " "))
      (end-of-line)
      (insert (s-concat " " (s-repeat padding-right "-") line-end))
      (forward-char))))

(defun comment-box-full ()
  "Comment region in a box spanning `fill-column'."

  (interactive)
  (let*
      ((start       (if (use-region-p)
                        (region-beginning) (line-beginning-position)))
       (end         (copy-marker
                     (if (use-region-p)
                         (region-end) (line-end-position)) t))
       (line-start  (comment-padright comment-start comment-add))
       (line-end    (comment-padleft comment-start comment-add)))

    (uncomment-region start end)
    (goto-char start)
    (open-line 1)
    (indent-according-to-mode)
    (comment-horizontal-line)
    (while (< (point) end)
      (indent-according-to-mode)
      (insert line-start)
      (end-of-line)
      (insert-char ? (- fill-column (current-column) (length line-end)))
      (insert line-end)
      (forward-line 1)))
    (open-line 1)
    (indent-according-to-mode)
    (comment-horizontal-line))

(provide 'comment)
;;; comment.el ends here

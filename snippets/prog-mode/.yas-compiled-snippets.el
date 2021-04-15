;;; Compiled snippets and support files for `prog-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'prog-mode
				 '(("todo" "`(when (eq\n	  (count-matches \"[^[:space:]]\"\n				  (line-beginning-position) (point)) 0)\n  comment-start)``comment-start` TO-DO: $0`comment-end`" "todo" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/prog-mode/todo" nil nil)
				   ("hline" "`(let* ((field-len (- 79 (current-column) (* 4 (length comment-start)) 2))\n	  (field-char (string-to-char\n				(replace-regexp-in-string\n				 \";\" \"-\"\n				 (substring comment-start -1)))))\n  (concat\n   comment-start comment-start \" \"\n   (make-string field-len field-char)\n   \" \" comment-start comment-start))`\n$0" "horizontal_line" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/prog-mode/horizontal_line" nil nil)
				   ("fixme" "`(when (eq\n	  (count-matches \"[^[:space:]]\"\n				  (line-beginning-position) (point)) 0)\n  comment-start)``comment-start` FIX-ME: $0`comment-end`" "fixme" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/prog-mode/fixme" nil nil)
				   ("cblock" "`(let* ((field-len (- 79 (current-column) (* 4 (length comment-start)) 2))\n	  (field-char (string-to-char\n				(replace-regexp-in-string\n				 \";\" \"-\"\n				 (substring comment-start -1)))))\n  (concat\n   comment-start comment-start \" \"\n   (make-string field-len field-char)\n   \" \" comment-start comment-start))`\n`comment-start``comment-start`  ${1:comment}\n`(let* ((field-len (- 79 (current-column) (* 4 (length comment-start)) 2))\n	  (field-char (string-to-char\n				(replace-regexp-in-string\n				 \";\" \"-\"\n				 (substring comment-start -1)))))\n  (concat\n   comment-start comment-start \" \"\n   (make-string field-len field-char)\n   \" \" comment-start comment-start))`\n$0" "comment_block" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/prog-mode/comment_block" nil nil)))


;;; Do not edit! File generated at Wed Apr 14 16:24:50 2021

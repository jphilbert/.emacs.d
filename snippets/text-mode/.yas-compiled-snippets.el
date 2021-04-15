;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
				 '(("today" "`(format-time-string \"%Y-%m-%d\")`" "(Today)" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/text-mode/today.yasnippet" nil nil)
				   ("time" "`(format-time-string \"%H:%M\")`\n\n" "(current time)" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/text-mode/time.yasnippet" nil nil)
				   ("thisFile" "`(file-name-nondirectory (buffer-file-name))`" "(This File)" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/text-mode/thisFile.yasnippet" nil nil)
				   ("thisDirectory" "`(file-name-directory (buffer-file-name))`" "(This Directory)" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/text-mode/thisDir.yasnippet" nil nil)))


;;; Do not edit! File generated at Wed Apr 14 16:24:50 2021

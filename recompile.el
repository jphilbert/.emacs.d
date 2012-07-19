;; ----- Scratch Buffer -----

(byte-compile-file "~/.emacs.d/packages/modes/Pymacs/pymacs.el")

(mapcar
 '(lambda (x) (byte-recompile-directory x 0))
 '("~/.emacs.d/packages/miscellaneous/auto complete"
   "~/.emacs.d/packages/modes/auctex"
   "~/.emacs.d/packages/modes/auctex/style"
   "~/.emacs.d/packages/modes/ess/lisp"
  ;; "~/.emacs.d/packages/modes/ess/lisp/tmp"
  ))

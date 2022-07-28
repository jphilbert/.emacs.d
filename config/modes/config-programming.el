;;; CONFIG-PROGRAMMING.EL --- prog-mode configuration

;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

;;; Code:

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in source code
(require 'hl-todo)
(global-hl-todo-mode 1)

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))


(defun config-mode-programming ()
  "Default coding hook, useful with any programming language."
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (smartparens-mode +1)
  (smartparens-global-strict-mode -1)
  (comment-auto-fill-only-comments-local)
  (hs-minor-mode))

(add-hook 'prog-mode-hook 'config-mode-programming)

(provide 'config-programming)
;;; CONFIG-PROGRAMMING.EL ends here

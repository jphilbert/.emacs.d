;;; config-functions.el --- Core Config functions.


;;; Code:
(require 'cl-lib)

(defun config-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun config-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory config-root 0))

(defun config-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))



(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern
                                (concat
                                 (symbol-name command) "-" advice-name))
                              activate)
                    ,@body))
               commands)))


(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))


(defcustom config-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'config)

(defcustom config-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'config)


;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) config-yank-indent-threshold)
      (indent-region beg end nil)))


(defun hs-toggle-hiding (&optional e)
  "Toggle hiding/showing of a block.
See `hs-hide-block' and `hs-show-block'.
Argument E should be the event that triggered this action."
  (interactive)
  (hs-life-goes-on
   ;; (posn-set-point (event-end e))
   (if (hs-already-hidden-p)
       (hs-show-block)
     (hs-hide-block))))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    (let ((c-reg (hs-inside-comment-p)))
      (if (and c-reg (nth 0 c-reg))
          ;; point is inside a comment, and that comment is hideable
          (goto-char (nth 0 c-reg))
        (end-of-line)
        ;; (message "%s" (hs-find-block-beginning))
        (when (and (not c-reg)
                   (hs-find-block-beginning)
		   (hs-looking-at-block-start-p))
          ;; point is inside a block
          ;; (message "point is inside a block")
          (goto-char (match-end 0)))))
    (end-of-line)
    (hs-overlay-at (point))))


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


(provide 'config-functions)
;;; config-functions.el ends here

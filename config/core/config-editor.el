;;; config-editor.el --- fundamental editor options

;;; Commentary:

;; Global / Fundamental Mode options for Emacs

;;; Code:


;;  '(abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
;;  '(ac-use-quick-help nil)
;;  '(confirm-kill-processes nil)

;; '(cua-mode t nil (cua-base))
;;  '(delete-by-moving-to-trash t)
;;  '(delete-selection-mode t)


;; '(mouse-avoidance-mode 'jump nil (avoid))
;; '(save-abbrevs nil)
;; '(select-enable-clipboard t)
;; '(tooltip-mode nil)



(require 'crux)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 5)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)


(setq comment-auto-fill-only-comments t)

(setq fill-column 80)

;; ------------------------------------------------------------------------- ;;
;; Directories / Backups
;; ------------------------------------------------------------------------- ;;
(setq-default 
 ;; ---------- Backups Directory and Naming ---------- ;;
 ;; [!] Backups (i.e. versions)
 backup-directory-alist			`((".*" . ,config-dir-backup))
 
 ;; [#] Auto-saves are done after altering but before saving
 auto-save-file-name-transforms
 `((".*"  ,(concat config-dir-autosave "/") t))
 
 ;; Auto-save List (for recover-session)
                                        ;auto-save-list-file-prefix (expand-file-name "auto-save-list" config-dir-save)
 auto-save-list-file-name
 (concat (expand-file-name "auto-save-list" config-dir-save)
         " "
         (format-time-string "%Y%m%d-%H%M%S")
    	    ".txt")
 ;; [.#] Locks are done in the same path (not to be confused with Auto-saves)
 create-lockfiles				nil

 ;; ---------- Backup Settings ---------- ;;
 version-control			t	; Use version numbers for backups.
 kept-new-versions			2	; Number of newest versions to keep.
 kept-old-versions			0	; Number of oldest versions to keep.
 delete-old-versions		t	; Don't ask to delete excess backup versions.
 backup-by-copying			t	; Copy all files, don't rename them.
 )


;; ---------- Create Backup After Each Save ---------- ;;
(advice-add 'backup-buffer :before #'(lambda () (setq buffer-backed-up nil)))
(add-hook 'before-save-hook  'backup-buffer)

;; ---------- Turn On Desktop Saving ---------- ;;
(setq-default
 desktop-path			(list config-dir-save)
 desktop-base-file-name		"desktop.el"
 desktop-base-lock-name		"desktop.lock.el"
 desktop-load-locked-desktop	t
 desktop-restore-eager		5
 desktop-restore-frames		t
 )

(desktop-save-mode)  

;; ---------- Opens Files with Emacs ---------- ;;
(require 'server)
(defun server-ensure-safe-dir (dir) t) ; Suppresses common windows error
(setq
 server-auth-dir	config-dir-save ; change the server directory
 server-name		(expand-file-name "server-process" config-dir-save))
;; !!! if changed, batch file needs option "-f %path/server_file%"
(server-force-delete)				; just in case delete
(server-start)						; start the server




;; ------------------------------------------------------------------------- ;;

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)




;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; smart pairing for all
(require 'smartparens-config)
;; (setq sp-base-key-bindings			'paredit)
(setq sp-autoskip-closing-pair		'always)
(setq sp-hybrid-kill-entire-symbol		nil)
;; (sp-use-paredit-bindings)
(show-smartparens-global-mode +1)




;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode 0)

(require 'rect)
(crux-with-region-or-line kill-region)

(defun comment-auto-fill-only-comments-local ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun config-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(setenv "LANG" "en_US")
(setq
 ispell-program-name			config-app-spell
 ispell-extra-args				'("--sug-mode=ultra")
 ispell-hunspell-dict-paths-alist	`(("en_US" ,config-app-spell-dict))
 ispell-local-dictionary-alist
 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))


(require 'flyspell)
;; :hook (text-mode . flyspell-mode))

(require 'flyspell-correct)
(require 'flyspell-correct-popup)


;; (defun config-enable-flyspell ()
;;   "Enable command `flyspell-mode' if `config-flyspell' is not nil."
;;   (when (and config-flyspell (executable-find ispell-program-name))
;;     (flyspell-mode +1)))

(defun config-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `config-clean-whitespace-on-save' is not nil."
  (when config-clean-whitespace-on-save
    (whitespace-cleanup)))


;; enable narrowing commands
(put 'narrow-to-region	'disabled nil)
(put 'narrow-to-page	'disabled nil)
(put 'narrow-to-defun	'disabled nil)

;; enabled change region case commands
(put 'upcase-region		'disabled nil)
(put 'downcase-region	'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer		'disabled nil)

(require 'expand-region)




(require 'recentf) 
(setq recentf-save-file
      (expand-file-name "recentf.el" config-dir-save))
(recentf-mode 1)


;; bookmarks
(require 'bookmark)
(setq
 bookmark-default-file	(expand-file-name
					 "bookmarks.txt" config-dir-save)
 bookmark-save-flag		1)


;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq
 dired-recursive-deletes		'always
 dired-recursive-copies		'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation


(require 'tabify)
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'rg)
(rg-enable-default-bindings)
(setq transient-history-file
      (expand-file-name "transient_history.el" config-dir-save))


;; supercharge your undo/redo with undo-tree
;; (require 'undo-tree)

;; diff-hl

;; easy-kill

;; smart-hungry-delete
(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)


(defadvice server-visit-files
    (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that
format.  Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg
   0
   (mapcar (lambda (fn)
             (let ((name (car fn)))
               (if (string-match
                    "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                   (cons
                    (match-string 1 name)
                    (cons (string-to-number (match-string 2 name))
                          (string-to-number (or (match-string 3 name) ""))))
                 fn))) files)))



;; ------------------------------------------------------------------------- ;;
;; Completion
;; ------------------------------------------------------------------------- ;;
(require 'vertico)
(require 'orderless)
(require 'vertico-directory)
(require 'marginalia)
(vertico-mode)

;; Configure a custom style dispatcher (see the Consult wiki)
;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;       orderless-component-separator #'orderless-escapable-split-on-space)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))


;; Tidy shadowed file names
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

;; Enable richer annotations using the Marginalia package
(marginalia-mode)

(require 'corfu)
(global-corfu-mode)

;; try the `completion-category-sort-function' first
(advice-add
 #'vertico--sort-function :before-until
 #'completion-category-sort-function)

(defun completion-category-sort-function ()
  (alist-get (vertico--metadata-get 'category)
             completion-category-sort-function-overrides))

(defvar completion-category-sort-function-overrides
  '((file . directories-before-files))
  "Completion category-specific sorting function overrides.")

(defun directories-before-files (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))


;; Hide-Show
(defvar-local hs-hide-all-state
  nil
  "The state of the folding of the buffer. `hs-toggle-hiding-all'")
(advice-add 'hs-hide-all
		  :after
		  #'(lambda () (setq hs-hide-all-state t))
		  '((name . "set hs-hide-all-state")))
(advice-add 'hs-show-all
		  :after
		  #'(lambda () (setq hs-hide-all-state nil))
		  '((name . "set hs-hide-all-state")))

(defun hs-toggle-all ()
  "Toggles the document's folding.

This simply does the opposite of what previously done in the buffer."
  (interactive)
  (if hs-hide-all-state
	 (hs-show-all)
    (hs-hide-all)))

(defun hs-toggle ()
  "Toggle hiding/showing of a block.
Fix for `hs-toggle-hiding'."
  (interactive)
  (hs-life-goes-on
   (if (hs-already-hidden-p)
       (hs-show-block)
     (hs-hide-block))))

(setq-default hs-hide-comments-when-hiding-all		nil)
(setq-default hs-allow-nesting					t)



(provide 'config-editor)

;;; CONFIG-EDITOR.EL ends here

;; (define-key prog-mode-map (kbd "M-(") (config-wrap-with "("))
;; ;; FIXME: pick terminal friendly binding
;; ;; (define-key prog-mode-map (kbd "M-[") (config-wrap-with "["))
;; (define-key prog-mode-map (kbd "M-\"") (config-wrap-with "\""))



;; TODO: move to config-mode-text
;; Auto-Fill text modes
(add-hook	'text-mode-hook	'turn-on-auto-fill)	
;; (add-hook 'text-mode-hook 'config-enable-flyspell)
;; (add-hook 'text-mode-hook 'config-enable-whitespace)
;; (add-hook 'text-mode-hook 'abbrev-mode)



;; TODO: move to config-mode-shell
;; ;; make a shell script executable automatically on save
;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))



(define-keys   prog-mode-map
  (kbd "<backspace>")    'smart-hungry-delete-backward-char
  (kbd "<delete>")       'smart-hungry-delete-forward-char)


(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))


(setq rg-executable 'config-app-ripgrep)


(define-keys vertico-map
  (kbd "RET")		'vertico-directory-enter
  (kbd "DEL")		'vertico-directory-delete-char
  (kbd "M-DEL")		'vertico-directory-delete-word)


(define-keys minibuffer-local-map
  (kbd "M-a")		'marginalia-cycle)


(setq projectile-cache-file
      (expand-file-name "projectile.cache" config-dir-save))
(setq projectile-known-projects-file
      (expand-file-name "projectile-known-projects.eld" config-dir-save))

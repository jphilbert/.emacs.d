;;; config-editor.el --- fundamental editor options

;;; Commentary:

;; Global / Fundamental Mode options for Emacs


;;; Code:
(require 'web-search)

;; ------------------------------------------------------------------------- ;;
;; Temporary Directories
;; ------------------------------------------------------------------------- ;;
;; ---------- Backups Directory and Naming ---------- ;;
;; [!] Backups (i.e. versions)
(make-directory (config-get :config-paths :backup) t)
(setq backup-directory-alist
	  `((".*" . ,(config-get :config-paths :backup))))

;; [#] Auto-saves are done after altering but before saving
(make-directory (config-get :config-paths :autosave) t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat (config-get :config-paths :autosave) "/") t)))

;; Auto-save List (for recover-session)
(make-directory (config-get :config-paths :temp) t)
(setq auto-save-list-file-name
      (concat (expand-file-name "auto-save-list"
                                (config-get :config-paths :temp))
              " "
              (format-time-string "%Y%m%d-%H%M%S")
              ".txt"))

;; [.#] Locks are done in the same path (not to be confused with Auto-saves)
(setq create-lockfiles				nil)

;; ---------- Backup Settings ---------- ;;
(setq
 version-control                t	; Use version numbers for backups.
 kept-new-versions          	2	; Number of newest versions to keep.
 kept-old-versions              0	; Number of oldest versions to keep.
 delete-old-versions            t	; Don't ask to delete excess backup versions.
 backup-by-copying              t	; Copy all files, don't rename them.
 )


;; ---------- Create Backup After Each Save ---------- ;;
(advice-add 'backup-buffer :before #'(lambda () (setq buffer-backed-up nil)))
(add-hook 'before-save-hook  'backup-buffer)

;; ---------- Turn On Desktop Saving ---------- ;;
(setq
 desktop-path                   (list (config-get :config-paths :temp))
 desktop-base-file-name         "desktop.el"
 desktop-base-lock-name         "desktop.lock.el"
 desktop-load-locked-desktop	t
 desktop-restore-eager          t
 desktop-restore-frames         nil
 desktop-clear-preserve-buffers nil)
(desktop-save-mode)
(add-hook 'desktop-after-read-hook 'frame-redisplay-all-file-buffers)

;; ---------- Opens Files with Emacs ---------- ;;
(require 'server)
;; change the server directory
(setq
 ;; change the server directory
 server-auth-dir	(config-get :config-paths :temp)
 ;; !!! if changed, batch file needs option "-f %path/server_file%"
 server-name		(expand-file-name
                     "server-process" (config-get :config-paths :temp)))

(server-force-delete)                   ; just in case delete
(server-start)                          ; start the server

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


(require 'crux)

;; '(cua-mode t nil (cua-base))
;; '(delete-by-moving-to-trash t)

;; No Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Newline at end of file
(setq require-final-newline t)

;; Delete the selection with a key press
(delete-selection-mode t)

(setq confirm-kill-processes nil)


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Minor mode to buttonize URLs and e-mail addresses
(goto-address-prog-mode t)

;; ---------- smart tab behavior - indent or complete ----------
(setq tab-always-indent                 'complete)

;; ---------- smart pairing for all ----------
(require 'smartparens-config)

(setq
 sp-autoskip-closing-pair           'always
 sp-hybrid-kill-entire-symbol		nil)

(show-smartparens-global-mode +1)

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))


;; disable annoying blink-matching-paren
(setq blink-matching-paren              nil)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style        'forward
      uniquify-separator                "/"
      ;; rename after killing uniquified
      uniquify-after-kill-buffer-p      t
      ;; don't muck with special buffers
      uniquify-ignore-buffers-re        "^\\*")


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

(defun auto-fill-only-comments-local ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode +1))

(defun config-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))



(set-default 'imenu-auto-rescan t)

;; ---------- Spell Checking ----------
(require 'flyspell)
(require 'flyspell-correct)
(require 'flyspell-correct-popup)
(setenv "LANG" "en_US")
(setq
 ispell-program-name                (config-get :applications :spell :exe)
 ispell-extra-args                  '("--sug-mode=ultra")
 ispell-hunspell-dict-paths-alist   `(("en_US" ,(config-get
                                                 :applications :spell :dict)))
 ispell-local-dictionary-alist      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]"
                                       "[']" nil ("-d" "en_US") nil utf-8)))


;; enable narrowing commands
(put 'narrow-to-region	'disabled nil)
(put 'narrow-to-page	'disabled nil)
(put 'narrow-to-defun	'disabled nil)

;; enabled change region case commands
(put 'upcase-region		'disabled nil)
(put 'downcase-region	'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer       'disabled nil)

(require 'expand-region)


;; Whitespace displaying and cleanup
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style
      '(face empty trailing lines-tail space-before-tab space-after-tab))
(setq
 comment-auto-fill-only-comments    t
 fill-column                        80)

(defun config-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `config-clean-whitespace-on-save' is not nil."
  (when config-clean-whitespace-on-save
    (whitespace-cleanup)))

(with-region-or-buffer indent-region)

(require 'tabify)
(with-region-or-buffer untabify)


(require 'recentf)
(setq recentf-save-file
      (expand-file-name "recentf.el" (config-get :config-paths :temp)))
(recentf-mode 1)


;; bookmarks
(require 'bookmark)
(setq bookmark-default-file
      (expand-file-name "bookmarks.txt" (config-get :config-paths :temp)))
(setq bookmark-save-flag      1)


;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file    'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes		'always)
(setq dired-recursive-copies		'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target            t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq
 ediff-window-setup-function        'ediff-setup-windows-plain
 ediff-split-window-function        'split-window-horizontally)


;; ---------- clean up obsolete buffers automatically ----------
(require 'midnight)

;; smarter kill-ring navigation


;; ---------- saner regex syntax ----------
(require 're-builder)
(setq reb-re-syntax 'string)

;; ---------- RipGREP ----------
;; https://rgel.readthedocs.io/en/2.2.1/index.html
(require 'rg)
(rg-enable-default-bindings)
(setq rg-executable                (config-get :applications :ripgrep))
(setq transient-history-file
      (expand-file-name "transient_history.el" (config-get :config-paths :temp)))


;; supercharge your undo/redo with undo-tree
;; (require 'undo-tree)

;; diff-hl

;; easy-kill

;; ---------- smart-hungry-delete ---------- 
(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

(defun config-smart-delete-backward (arg)
  "see `smart-hungry-delete-backward-char"
  (interactive "P")
  (if (use-region-p)
      (delete-region
       (min (mark) (point))
       (max (mark) (point)))
    (smart-hungry-delete-backward-char arg)))

(defun config-smart-delete-forward (arg)
  "see `smart-hungry-delete-forward-char"  
  (interactive "P")
  (if (use-region-p)
      (delete-region
       (min (mark) (point))
       (max (mark) (point)))
    (smart-hungry-delete-forward-char arg)))

(defun config-delete-whole-line ()
  (interactive)
  (kill-whole-line)
  (set-transient-map
   (let ((kmap (make-sparse-keymap)))
     (define-key kmap (kbd "<backspace>")   'config-delete-whole-line)
     (define-key kmap (kbd "<delete>")      'config-delete-whole-line)
     kmap)))

(defun config-smart-delete-line-backward ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode)
  (set-transient-map
   (let ((kmap (make-sparse-keymap)))
     (define-key kmap (kbd "<backspace>")   'config-delete-whole-line)
     (define-key kmap (kbd "<delete>")      'config-delete-whole-line)
     kmap)))

(defun config-smart-delete-line-forward ()
  (interactive)
  (kill-line nil)
  (set-transient-map
   (let ((kmap (make-sparse-keymap)))
     (define-key kmap (kbd "<backspace>")   'config-delete-whole-line)
     (define-key kmap (kbd "<delete>")      'config-delete-whole-line)
     kmap)))



;; ---------- operate-on-number ---------- ;;
(require 'operate-on-number)
(require 'smartrep)

;; ---------- Hide-Show ----------
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

(setq hs-hide-comments-when-hiding-all		nil
      hs-allow-nesting                      t)

;; show the name of the current function definition in the modeline
;; (require 'which-func)

;; font-lock annotations like TODO in source code
(require 'hl-todo)
(global-hl-todo-mode 1)


;; (require 'projectile)
;; (fmakunbound 'project-root)
(setq projectile-cache-file
      (expand-file-name "projectile.cache"
                        (config-get :config-paths :temp)))
(setq projectile-known-projects-file
      (expand-file-name "projectile-known-projects.eld"
                        (config-get :config-paths :temp)))



;; ------------------------------------------------------------------------- ;;
;; Completion
;; ------------------------------------------------------------------------- ;;
(require 'vertico)
(require 'orderless)
(require 'vertico-directory)
(vertico-mode)

(setq-default completion-ignore-case			t)

;; Configure a custom style dispatcher (see the Consult wiki)
;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;       orderless-component-separator #'orderless-escapable-split-on-space)
(setq
 completion-styles              '(orderless)
 completion-category-defaults   nil
 completion-category-overrides  '((file (styles partial-completion))))


;; Tidy shadowed file names
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)




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



;; ---------- Consult ---------- ;;
(require 'consult)

(setq consult-buffer-sources
      '(
        consult--source-hidden-buffer
        consult--source-modified-buffer
        consult--source-file-buffer
        consult--source-special-buffer
        ;; consult--source-buffer
        consult--source-recent-file
        consult--source-bookmark
        ;; consult--source-project-buffer
        ;; consult--source-project-recent-file
        ))

(setq consult--source-file-buffer
      `(:name     "File Buffer"
        :narrow   ?b
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :default  t
        :items
        ,(lambda () (consult--buffer-query
                :sort       'alpha
                :as         #'buffer-name
                :exclude    "\*.*"
                ))))

(setq consult--source-special-buffer
      `(:name     "Special Buffer"
        :narrow   ?s
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :items
        ,(lambda () (consult--buffer-query
                :sort       'alpha
                :as         #'buffer-name
                :include    "\*.*"
                ))))

(setq consult--source-recent-file
  `(:name       "Recent File"
    :narrow     ?f
    :category   file
    :face       consult-file
    :history    file-name-history
    :state      ,#'consult--file-state
    :new        ,#'consult--file-action
    :enabled    ,(lambda () recentf-mode)
    :hidden     t
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash)))
         (mapcar #'abbreviate-file-name
                 (seq-remove (lambda (x) (gethash x ht)) recentf-list))))))

;; TODO replace FIND with CONSULT-LINE
;; (defun consult-line-symbol-at-point ()
;;   (interactive)
;;   (consult-line (thing-at-point 'symbol)))

;; TODO
;; https://github.com/minad/consult/wiki#emacs-and-web-colors-list


;; ---------- Marginalia ---------- ;;
;; Enable richer annotations using the Marginalia package
(require 'marginalia)
(marginalia-mode)

(defun marginalia--annotate-local-file (cand)
  "Annotate local file CAND."
  (when-let (attrs (ignore-errors
                     ;; may throw permission denied errors
                     (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer)))
    (marginalia--fields
     ((file-size-human-readable (file-attribute-size attrs))
      :face 'marginalia-size :width -10)
     ((marginalia--time (file-attribute-modification-time attrs))
      :face 'marginalia-date :width -20))
    ))

(defun marginalia--buffer-status (buffer)
  "Return the status of BUFFER as a string."
  (format-mode-line
   '((:propertize "%1&" face marginalia-modified)
     marginalia-separator
     (10 (-20 (:propertize mode-name face marginalia-mode))))
   nil nil buffer))

(defun marginalia--buffer-file (buffer)
  "Return the file or process name of BUFFER."
  (if-let (proc (get-buffer-process buffer))
      (format
       "(%s %s) %s"
       proc (process-status proc)
       (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
    
    (abbreviate-file-name
     (cond
      ;; see ibuffer-buffer-file-name
      ((buffer-file-name buffer)
       (file-name-directory (buffer-file-name buffer)))
      ;; ((file-name-directory (buffer-file-name buffer)))
      ((when-let (dir (and (local-variable-p 'dired-directory buffer)
                           (buffer-local-value 'dired-directory buffer)))
         (expand-file-name (if (stringp dir) dir (car dir))
                           (buffer-local-value 'default-directory buffer))))
      ((local-variable-p 'list-buffers-directory buffer)
       (buffer-local-value 'list-buffers-directory buffer))
      ("")))))

(defun marginalia-annotate-buffer (cand)
  "Annotate buffer CAND with modification status, file name and major mode."
  (when-let (buffer (get-buffer cand))
    (marginalia--fields
     ((marginalia--buffer-status buffer))
     ((marginalia--buffer-file buffer)
      :truncate -0.75 :face 'marginalia-file-name))))


;; ---------- Corfu ---------- ;;
(require 'corfu)
(global-corfu-mode)

;; ---------- Tempel ---------- ;;
;; https://github.com/minad/tempel/index
(require 'tempel)
(setq tempel-path (config-get :config-paths :tempel))

;; ---------- Cape ---------- ;;
;; https://github.com/minad/cape
(require 'cape)
(setq
 completion-at-point-functions
 `(,(cape-super-capf
     #'elisp-completion-at-point
	 #'tempel-complete)
   cape-file))




;; (setq marginalia-annotator-registry
;;       (assq-delete-all 'buffer marginalia-annotator-registry))

;; (add-to-list 'marginalia-annotator-registry
;;              '(buffer config-marginalia-annotate-buffer))



;; hippie expand is dabbrev expand on steroids
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-all-abbrevs
;;         try-expand-list
;;         try-expand-line
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))





(provide 'config-editor)

;;; CONFIG-EDITOR.EL ends here






















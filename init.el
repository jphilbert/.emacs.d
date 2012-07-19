;; ----------------------------------------------------------------------------
;; Init.el (Global Options)
;; ----------------------------------------------------------------------------
(require 'cl)
(defvar *emacs-load-start* (current-time))
;; maps path of emacs files
(load "~/.emacs.d/packages/miscellaneous/tiny-tools/lisp/tiny/tinypath")
(require 'user-info)
;; ------------------------------ Header --------------------------------------

;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------
(setq-default
 auto-fill-function			'do-auto-fill	; turn on auto fill 
 comment-auto-fill-only-comments	t	; but only for comments
 inhibit-startup-screen			t	; No Splash Screen
 visible-bell				nil	; No Beep
 skeleton-pair				t  	; Auto pair matching
 fill-column				80
 x-select-enable-clipboard		t
 redisplay-dont-pause			t
 scroll-preserve-screen-position	1	; Keeps cursor in one spot
 delete-old-versions			t)	; delete backups

(cua-mode			nil)	; CUA mode
;; (desktop-save-mode		t)	; Reload previous files
(fset 'yes-or-no-p 'y-or-n-p)		; Simplify Questions

;; Prevent annoying "Active processes exist" query when you quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq-default ediff-split-window-function 'split-window-horizontally)

(setq-default
 initial-scratch-message
 ";; ----- Scratch Buffer ----- ")

;; -----------------------------------------------------------------------------
;; Directories / Backups
;; -----------------------------------------------------------------------------
(make-directory "~/.emacs.d/server/" t)
(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosaves/" t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))


;; -----------------------------------------------------------------------------
;; Opens Files with Emacs (if Emacs is running)
;; -----------------------------------------------------------------------------
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress common windows error
(server-start)


;; -----------------------------------------------------------------------------
;; Emacs Aesthetics
;; -----------------------------------------------------------------------------
(require 'aesthetics nil t)
(require 'multi-window nil t)
(require 'switch-frame nil t)


;; ----------------------------------------------------------------------------
;; Tiny Tools
;; ----------------------------------------------------------------------------
;; (require 'tinybuffer nil t)		
(require 'tinyef nil t)
(require 'tinyeat nil t)
(add-hook 'tinyef-load-hook 'tinyef-minibuffer-define-key-extras)
(setq tinyeat--load-hook '(tinyeat-install))


;; ----------------------------------------------------------------------------
;; Auto Indent
;; ----------------------------------------------------------------------------
(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode nil t)
;; (auto-indent-global-mode)

(autoload 'auto-indent-yank "auto-indent-mode" "" t)
(autoload 'auto-indent-yank-pop "auto-indent-mode" "" t)

(define-key global-map [remap yank] 'auto-indent-yank)
(define-key global-map [remap yank-pop] 'auto-indent-yank-pop)

(autoload 'auto-indent-delete-char "auto-indent-mode" "" t)
(define-key global-map [remap delete-char] 'auto-indent-delete-char)

(autoload 'auto-indent-kill-line "auto-indent-mode" "" t)
(define-key global-map [remap kill-line] 'auto-indent-kill-line)


;; ----------------------------------------------------------------------------
;; Aspell
;; ----------------------------------------------------------------------------
(setq-default ispell-program-name "aspell.exe")
(setq ispell-list-command "list"
      ispell-extra-args '("--sug-mode=fast")
      text-mode-hook '(lambda() (flyspell-mode t)))


;; ----------------------------------------------------------------------------
;; Completion
;; ----------------------------------------------------------------------------
(require 'auto-complete-config nil t)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/packages/auto complete/ac-dict")
(ac-config-default)
(setq-default ac-ignore-case nil)
(setq-default ac-auto-show-menu nil)
(setq ac-auto-show-menu t)
(define-key ac-completing-map (kbd "<tab>") 'ac-expand)
(define-key ac-completing-map [(return)] 'ac-complete)

(require 'icicles nil t)
(setq icicle-show-Completions-help-flag		nil
      icicle-candidate-width-factor		100
      icicle-Completions-display-min-input-chars 2
      completion-ignore-case			t)
(icy-mode t)


;; ----------------------------------------------------------------------------
;; Buffer Control
;; ----------------------------------------------------------------------------
;; Remove Needless Buffers
;;	Removes all *--* buffers after 5s of idle and others every 1d
;;	Active process / unsaved work / etc is safe
(require 'midnight)
(setq clean-buffer-list-kill-regexps
      '("^\\*.*\\*$"))
(setq clean-buffer-list-delay-special 0)
(setq clean-buffer-list-delay-general 1)
(run-with-idle-timer 5 5 'clean-buffer-list)

;; rewrote function without messages
(defun clean-buffer-list ()
  "Kill old buffers that have not been displayed recently.
The relevant variables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-never-regexps'.
While processing buffers, this procedure displays messages containing
the current date/time, buffer name, how many seconds ago it was
displayed (can be nil if the buffer was never displayed) and its
lifetime, i.e., its \"age\" when it will be purged.

JPH: Removed periodic message"
  (interactive)
  (let ((tm (float-time)) bts (ts (format-time-string "%Y-%m-%d %T"))
        delay cbld bn)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (setq bts (midnight-buffer-display-time buf) bn (buffer-name buf)
              delay (if bts (- tm bts) 0) cbld (clean-buffer-list-delay bn))
        ;; (message "[%s] `%s' [%s %d]" ts bn (if bts (round delay)) cbld)      
        (unless (or (midnight-find bn clean-buffer-list-kill-never-regexps
                                   'string-match)
                    (midnight-find bn clean-buffer-list-kill-never-buffer-names
                                   'string-equal)
                    (get-buffer-process buf)
                    (and (buffer-file-name buf) (buffer-modified-p buf))
                    (get-buffer-window buf 'visible) (< delay cbld))
          ;; (message "[%s] killing `%s'" ts bn)
          (kill-buffer buf))))))


;; ----------------------------------------------------------------------------
;; Snippets
;; ----------------------------------------------------------------------------
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/miscellaneous/yasnippet/snippets")
(setq yas/prompt-functions '(yas/completing-prompt))

(setq yas/trigger-key (kbd "SPC"))
(add-hook 'yas/minor-mode-on-hook 
          '(lambda () 
             (define-key yas/minor-mode-map yas/trigger-key 'yas/expand)))


;; ----------------------------------------------------------------------------
;; Folding
;; ----------------------------------------------------------------------------
(require 'fold-dwim nil t)
(setq-default hs-hide-comments-when-hiding-all nil)
(setq-default hs-allow-nesting t)

;; Toggle Folding on Whole Buffer
(defvar fold-dwim-toggle-all-state nil
  "Saves the state of the folding of the document.")

(make-variable-buffer-local 'fold-dwim-toggle-all-state)

(defun fold-dwim-toggle-all ()
  "Toggles the document's folding.  This is not smart in that it
does the opposite of what it did last so it may be wrong if
`fold-dwim-show-all' or `fold-dwim-hide-all' was executed with
out it knowing."
  (interactive)
  (if fold-dwim-toggle-all-state
      (fold-dwim-show-all)
    (fold-dwim-hide-all))
  (setq fold-dwim-toggle-all-state
	(not fold-dwim-toggle-all-state)))


;; ----------------------------------------------------------------------------
;; Buffer Window
;; ----------------------------------------------------------------------------
(setq-default bs-attributes-list
	      '((""		1 1 left bs--get-marked-string)
		("M"		1 1 left bs--get-modified-string)
		("R"		2 2 left bs--get-readonly-string)
		("Buffer"	bs--get-name-length 10 left bs--get-name)
		(""		2 2 left "  ")
		("Size"		8 8 right bs--get-size-string)
		(""		2 2 left "  ")
		("Mode"		16 16 middle bs--get-mode-name)
		(""		2 2 left "  ")))


;; --------------------------------------------------------------------------
;; GIT
;; --------------------------------------------------------------------------
;; just don't know how to use this yet...
;; (require 'egg nil t)
;; (require 'gist nil t)


;; ----------------------------------------------------------------------------
;; Key Binding (global)
;; ----------------------------------------------------------------------------
(require 'keybinding nil t)


;; ----------------------------------------------------------------------------
;; Particular Modes
;;	Note: the order is important
;; ----------------------------------------------------------------------------
(require 'lisp-setup nil t)
(require 'sql-setup nil t)
(require 'r-setup nil t)
;; (require 'latex-setup nil t)
;; (require 'python-setup nil t)
(require 'shell-setup nil t)
(require 'powershell-setup nil t)

;; Common terminal defaults
(setq-default
 ansi-color-for-comint-mode             t
 comint-scroll-to-bottom-on-input	t
 comint-scroll-to-bottom-on-output	t
 comint-move-point-for-output           t
 comint-prompt-read-only                nil)

;; ----------------------------------------------------------------------------
;; Functions (may put in seperate file)
;; ----------------------------------------------------------------------------
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and
current line is not blank and we are not at the end of the line, then comment
current line. Replaces default behaviour of comment-dwim, when it inserts
comment at the end of the line."
  ;; Original idea from
  ;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; ------------------------------ Footer ---------------------------------------
(message "init.el loaded in %ds"
	 (destructuring-bind (hi lo ms) (current-time)
	   (- (+ hi lo)
	      (+ (first *emacs-load-start*)
		 (second *emacs-load-start*)))))

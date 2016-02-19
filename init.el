;; ----------------------------------------------------------------------------
;; init.el (Global Options)
;; ----------------------------------------------------------------------------
(require 'cl)
(setq start-time (current-time))

;; maps path of emacs files
(let ((default-directory "~/.emacs.d/elpa/user/"))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/settings/")
(add-to-list 'load-path "~/.emacs.d/settings/modes/")

(require 'user-info)


;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------
(setq-default
 comment-auto-fill-only-comments	t	; but only for comments
 inhibit-startup-screen			t	; No Splash Screen
 visible-bell					t	; No Beep
 skeleton-pair					t  	; Auto pair matching
 fill-column					80
 x-select-enable-clipboard		t
 redisplay-dont-pause			t
 scroll-preserve-screen-position	1	; Keeps cursor in one spot
 delete-old-versions			t	; delete backups
 )

(add-hook
 'text-mode-hook 'turn-on-auto-fill)		; Auto-Fill (Comments Only)
(cua-mode			t)				; CUA mode
;; (desktop-save-mode		t)		; Reload previous files
(defalias 'yes-or-no-p 'y-or-n-p)		; Simplify Questions


;; Prevent annoying "Active processes exist" query when you quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq-default ediff-split-window-function 'split-window-horizontally)

;; Scratch Buffer
(setq-default
 initial-scratch-message
 ";; --------------- Scratch Buffer ---------------
"
 initial-major-mode 'emacs-lisp-mode)

;; Common terminal defaults
(setq-default
 ansi-color-for-comint-mode             t
 comint-scroll-to-bottom-on-input	t
 comint-scroll-to-bottom-on-output	t
 comint-move-point-for-output           t
 comint-prompt-read-only                nil)

;; Change Tab Stops
(setq-default tab-width 5)
(setq-default tab-stop-list (number-sequence 5 120 5))


(message "SETTING - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(message "PACKAGES - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Directories / Backups
;; -----------------------------------------------------------------------------
(make-directory "~/.emacs.d/server/" t)
(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosaves/" t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))))

(message "DIRECTORIES / BACKUPS - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Opens Files with Emacs (if Emacs is running)
;; -----------------------------------------------------------------------------
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress common windows error
(server-start)

(message "SERVER - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; ----------------------------------------------------------------------------
;; Tiny Tools
;; ----------------------------------------------------------------------------
;; (require 'tinybuffer nil t)		
(require 'tinyef)
(require 'tinyeat)
(add-hook 'tinyef-load-hook 'tinyef-minibuffer-define-key-extras)
(setq tinyeat--load-hook '(tinyeat-install))


;; ----------------------------------------------------------------------------
;; Aspell
;; ----------------------------------------------------------------------------
(setq-default ispell-program-name "aspell.exe")
(setq ispell-list-command "list"
      ispell-extra-args '("--sug-mode=fast"))
(add-hook 'text-mode-hook
	  '(lambda() (flyspell-mode t)))

(message "TINY TOOLS / ASPELL - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))


;; ----------------------------------------------------------------------------
;; Completion
;; ----------------------------------------------------------------------------
;; Auto Complete
(require 'auto-complete-config)
(require 'my-pos-tip)			; For better Tool Tips

(ac-config-default)
(global-auto-complete-mode t)
(setq-default ac-quick-help-delay	0.8
	      ac-show-menu-immediately-on-auto-complete nil
	      ac-sources (append ac-sources '(ac-source-yasnippet
					      ac-source-filename))
	      ;; (setq-default ac-ignore-case nil)
	      ;; (setq-default ac-auto-show-menu nil)
	      ac-auto-show-menu		nil
	      ac-auto-start		2)
(ac-flyspell-workaround)

(define-key ac-completing-map (kbd "<tab>")	'ac-next)
(define-key ac-completing-map [(return)]	'ac-complete)


;; Icicles
(require 'icicles)
(setq icicle-show-Completions-help-flag		nil
      icicle-candidate-width-factor		100
      icicle-Completions-display-min-input-chars 2
      completion-ignore-case			t
      icicle-change-region-background-flag	t
      icicle-region-background			"black")
(icy-mode t)

(message "AC / ICICLES - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; ----------------------------------------------------------------------------
;; Dired+
;; ----------------------------------------------------------------------------
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(diredp-make-find-file-keys-reuse-dirs)


;; ----------------------------------------------------------------------------
;; Buffer Control
;; ----------------------------------------------------------------------------
;; Remove Needless Buffers
;;	Removes all *--* buffers after 5s of idle and others every 1d
;;	Active process / unsaved work / etc is safe
(require 'midnight)
(setq clean-buffer-list-kill-regexps	'("^\\*.*\\*$")
      clean-buffer-list-delay-special	0
      clean-buffer-list-delay-general	1)
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
(yas-global-mode 1)

(message "YAS - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; ----------------------------------------------------------------------------
;; Folding
;; ----------------------------------------------------------------------------
(require 'fold-dwim)
(setq-default hs-hide-comments-when-hiding-all	nil
	      hs-allow-nesting			t)

;; (add-to-list 'hs-special-modes-alist '(css-mode "{" "}" "/[*/]" nil nil))

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

(message "DWIM / BUFFER - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; --------------------------------------------------------------------------
;; GIT
;; --------------------------------------------------------------------------
;; just don't know how to use this yet...
;; (require 'egg nil t)
;; (require 'gist nil t)


;; -----------------------------------------------------------------------------
;; Miscellaneous Packages
;; -----------------------------------------------------------------------------
(require 'expand-region)		; Expand regions
(require 'websearch)		; Search web functionality
(require 'misc)			; Miscellaneous User created functions

(message "MISC PACK - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------
(require 'lisp-setup nil t)
(require 'sql-setup nil t)
(require 'r-setup nil t)
;; (require 'latex-setup nil t)
(require 'web-setup nil t)
(require 'python-setup nil t)
(require 'shell-setup nil t)
(require 'powershell-setup nil t)

(message "MODES - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Key-Binding
;; -----------------------------------------------------------------------------
(require 'keybinding nil t)	; General Key-binding Setup
(require 'mouse3)			; Additional Mouse Button functions


;; -----------------------------------------------------------------------------
;; Aesthetics
;; -----------------------------------------------------------------------------
(require 'aesthetics)		; Near the end of file (due to mode dependencies
						; among other things)
(require 'frame-settings)	; Setup frames

(message "KEYBINDING / AESTHETICS - Time: %.03fs" (float-time (time-since start-time)))
(setq start-time (current-time))

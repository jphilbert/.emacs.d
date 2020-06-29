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

(require 'user)


;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------
(setq-default
 comment-auto-fill-only-comments	t	; but only for comments
 inhibit-startup-screen			t	; No Splash Screen
 visible-bell					t	; No Beep
 fill-column					80
 x-select-enable-clipboard		t
 redisplay-dont-pause			t
 scroll-preserve-screen-position	1	; Keeps cursor in one spot
 delete-old-versions			t	; delete backups
 delete-by-moving-to-trash		t	; use recycling bin
 comint-use-prompt-regexp		t	; fixes the weird prompt highlighting
 )

(add-hook
 'text-mode-hook 'turn-on-auto-fill)	; Auto-Fill (Comments Only)
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
;; (setq-default tab-stop-list (number-sequence 5 120 5))

(message "SETTING - Time: %.03fs\n" (float-time (time-since start-time)))
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

(message "PACKAGES - Time: %.03fs\n" (float-time (time-since start-time)))
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

(message "DIRECTORIES / BACKUPS - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Opens Files with Emacs (if Emacs is running)
;; -----------------------------------------------------------------------------
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress common windows error
(server-start)

(message "SERVER - Time: %.03fs\n" (float-time (time-since start-time)))
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
(add-to-list 'exec-path "C:/ProgramData/Aspell/bin") ;if not in path
(setq-default ispell-program-name "aspell.exe")
(setq ispell-list-command "list"
      ispell-extra-args '("--sug-mode=fast"))
(add-hook 'text-mode-hook
	  '(lambda() (flyspell-mode t)))

(message "TINY TOOLS / ASPELL - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

;; ----------------------------------------------------------------------------
;; Completion
;; ----------------------------------------------------------------------------
;; Auto Complete
(require 'auto-complete-config)
(require 'my-pos-tip)			; For better Tool Tips

(ac-config-default)
(global-auto-complete-mode t)
(setq-default
 ac-quick-help-delay		5
 ac-show-menu-immediately-on-auto-complete nil
 ac-sources				(append ac-sources '(ac-source-yasnippet
										 ac-source-filename))
 ac-ignore-case			nil
 ;; (setq-default ac-auto-show-menu nil)
 ac-auto-show-menu			nil
 ac-auto-start				2)
(ac-flyspell-workaround)

(define-key ac-completing-map (kbd "<tab>")	'ac-next)
(define-key ac-completing-map [(return)]	'ac-complete)

(message "AC - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))


;; Icicles
(require 'icicles)
(setq icicle-show-Completions-help-flag		nil
      icicle-candidate-width-factor		100
      icicle-Completions-display-min-input-chars 2
      completion-ignore-case			t
      icicle-change-region-background-flag	t
      icicle-region-background			"black")
(icy-mode t)

(message "ICICLES - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))


;; Abbrev-Mode
(setq save-abbrevs nil)
(setq abbrev-file-name				;; tell emacs where to read abbrev
	 "~/.emacs.d/abbrev_defs")		;; definitions from...

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

;; advise to remove messages
(defun my-clean-buffer-list (orig-fun &rest args)
  (cl-letf (((symbol-function 'message) #'format))
    (apply orig-fun args)))

(advice-add 'clean-buffer-list :around #'my-clean-buffer-list)


;; ----------------------------------------------------------------------------
;; Snippets
;; ----------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)

(message "YAS - Time: %.03fs\n" (float-time (time-since start-time)))
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


(message "DWIM / BUFFER - Time: %.03fs\n" (float-time (time-since start-time)))
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

(message "MISC PACK - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

;; -----------------------------------------------------------------------------
;; Key-Binding
;; -----------------------------------------------------------------------------
(require 'keybinding nil t)	; General Key-binding Setup
(require 'mouse3)			; Additional Mouse Button functions

;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------
(require 'lisp-setup nil t)
(message "LISP-SETUP - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

(require 'sql-setup nil t)
(message "SQL-SETUP - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

(require 'r-setup nil t)
(message "R-SETUP - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

(require 'python-setup nil t)
(message "PYTHON-SETUP - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))

;; (require 'latex-setup nil t)
(require 'web-setup nil t)
(require 'shell-setup nil t)
(require 'powershell-setup nil t)

(require 'markdown-mode)
(setq-default markdown-command "multimarkdown")
(add-hook 'markdown-mode-hook
		'(lambda()
		   (auto-fill-mode 0)
		   (setq tab-width 2)
		   (setq fill-column 99999999)
		   (push '(?`. ?`)
                    (getf autopair-extra-pairs :everywhere))))
(define-many-keys markdown-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]		'markdown-preview
  
  ;; ---------- Styles ----------
  (kbd "C-i")			'markdown-insert-italic
  (kbd "C-b")			'markdown-insert-bold
  (kbd "C-l")			'markdown-insert-link
  (kbd "C-j")			'markdown-insert-code
  

  ;; ---------- Hide-Show ----------
  [(f3)]			     'markdown-hide-subtree
  [(shift f3)]           'markdown-show-subtree
  
  ;; ---------- Help ----------
  "\C-hf"      	'(lambda ()
				   (interactive)
				   (browse-url
   "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")
				   )
  [(S-f1)]		'(lambda ()
				   (interactive)
				   (browse-url
   "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")
				   )
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "markdown "))
  )

(message "MISC MODES - Time: %.03fs\n" (float-time (time-since start-time)))
(setq start-time (current-time))


;; -----------------------------------------------------------------------------
;; Aesthetics
;; -----------------------------------------------------------------------------
(require 'aesthetics)		; Near the end of file (due to mode dependencies
						; among other things)
(require 'frame-settings)	; Setup frames

(message "AESTHETICS - Time: %.03fs\n" (float-time (time-since start-time)))

(desktop-save-mode 1)
(setq-default desktop-load-locked-desktop t)

(setq start-time (current-time))

;; Tooltip-mode has been causing lag in various modes so just disabling it
(tooltip-mode -1)

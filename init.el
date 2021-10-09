;; ------------------------------------------------------------------------- ;;
;; Package Setup
;; ------------------------------------------------------------------------- ;;
;; Add Packages in ~/.emacs.d/lisp Directory
(defconst user-package-dir
  (expand-file-name (concat user-emacs-directory "lisp/"))
  "Location to place file backups, auto-saves, and recovery file list")
(normal-top-level-add-to-load-path
 (directory-files
  user-package-dir t
  ;; Folders (basically anything with out a . (except _this_ directory))
  "\\`[[:alnum:]_-]+\\'\\|\\`\\.\\'"))

;; Load USE-PACKAGE - Time: 4%
(eval-when-compile
  (require 'use-package))

;; Time: 10%
(use-package package
  :config
  (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))


 
;; ------------------------------------------------------------------------- ;;
;; Directories / Backups
;; ------------------------------------------------------------------------- ;;
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/
(defconst emacs-backup-dir
  (let* ((dir "backups/")
	    (dir (concat user-emacs-directory dir)))
    (make-directory dir t)
    dir)
  "Location to place file backups, auto-saves, and recovery file list")

(setq-default
 ;; ---------- Backups Directory and Naming ---------- ;;
 ;; [!] Backups (i.e. versions)
 backup-directory-alist			`((".*" . ,emacs-backup-dir))
 ;; [#] Auto-saves are done after altering but before saving
 auto-save-file-name-transforms    `((".*"  ,emacs-backup-dir t))
 ;; Auto-save List (for recover-session)
 auto-save-list-file-prefix		(concat emacs-backup-dir
								   "auto-save-list ")
 auto-save-list-file-name		(concat emacs-backup-dir
								   "auto-save-list "
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

 ;; ---------- Desktop Saving ---------- ;;
 desktop-path				 `(,emacs-backup-dir "~")
 ;; desktop-base-file-name
 desktop-load-locked-desktop	t
 desktop-restore-eager		5
 desktop-restore-frames		nil
 )

;; ---------- Create Backup After Each Save ---------- ;;
(advice-add 'backup-buffer :before #'(lambda () (setq buffer-backed-up nil)))
(add-hook 'before-save-hook  'backup-buffer)

;; ---------- Turn On Desktop Saving ---------- ;;
(desktop-save-mode)  

;; ---------- Opens Files with Emacs ---------- ;;
(use-package server
  :config
  (defun server-ensure-safe-dir (dir) t) ; Suppresses common windows error
  
  (setq server-auth-dir emacs-backup-dir) ; change the server directory
  ;; !!! if changed, batch file needs option "-f %path/server_file%"

  (server-force-delete)				; just in case delete
  (server-start)					; start the server
  )


;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------
(setq-default
 user-full-name				"John P. Hilbert"
 user-mail-address				"jphilbert@gmail.com"
 
 fill-column					80
 tab-width					5
 comment-auto-fill-only-comments	t	; Don't fill except for comments
 inhibit-startup-screen			t	; No Splash Screen
 visible-bell					t	; No Beep
 x-select-enable-clipboard		t	; yank to MS clipboard
 ;; redisplay-dont-pause			t	; > obsolete <
 scroll-preserve-screen-position	1	; Keeps cursor in one spot
 delete-by-moving-to-trash		t	; use recycling bin
 ediff-split-window-function		'split-window-horizontally
 
 completion-ignore-case			t
 
 ;; ---------- Scratch ---------- ;;
 initial-scratch-message		";; ---------- Scratch Buffer ---------- ;;\n"
 initial-major-mode				'emacs-lisp-mode
 
 ;; ---------- Command Interpreter ---------- ;;
 ansi-color-for-comint-mode        t
 comint-use-prompt-regexp		t	; fixes the weird prompt highlighting
 comint-scroll-to-bottom-on-input	t
 comint-scroll-to-bottom-on-output	t
 comint-move-point-for-output		t
 comint-prompt-read-only			nil

 ;; ---------- Abbreviations ---------- ;;
 save-abbrevs					nil
 abbrev-file-name				(concat user-emacs-directory "abbrev_defs")
 directory-abbrev-alist
(append
  directory-abbrev-alist
  '(("~/desktop/"
	.	 "~/OneDrive - UPMC/desktop/")
    ("~/custom/"
	.	"X:/Data Analysis/Data Analysis(Custom)/")
    ("~/dohe/"
	.	"X:/Data Analysis/Data Analysis(DeptUsers)/")
    ("~/dev/"
	.	"X:/Data Analysis/Data Analysis(DeptUsers)/Development Team/")
    ("~/sci/"
	.	"X:/Data Analysis/Data Analysis(DeptUsers)/Science_Team/")))
 
 )

;; Substitute y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prevent annoying "Active processes exist" query when you quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


;; Auto-Fill text modes
(add-hook	'text-mode-hook	'turn-on-auto-fill)	

;; enable PRETTY-LAMBDA-MODE
(add-to-list 'pretty-lambda-auto-modes 'python-mode)
(pretty-lambda-for-modes)		

;; Various Minor Modes
;; TO-DO: convert to customized variables
(tooltip-mode				0)		; been causing lag in various modes  
(cua-mode					t)	
(fringe-mode                  0)	     ; Removes fringes
(global-visual-line-mode		t)	     ; Word Wrapping
(global-font-lock-mode		t)	     ; Syntax Coloring
(delete-selection-mode		t)	     ; Entry deletes marked text
(show-paren-mode			t)	     ; Highlight pairs
(mouse-avoidance-mode		'jump)    ; Moves cursor out of way
(tool-bar-mode				0)	     ; No Tool Bar
(menu-bar-mode				0)	     ; No Menu Bar
(toggle-scroll-bar			nil)	     ; no scroll bars
(electric-pair-mode)				; turn on auto pairing






;; -----------------------------------------------------------------------------
;; Aesthetics
;; -----------------------------------------------------------------------------
(use-package aesthetics)				; Time: 9%
(use-package frame-settings)			; Setup frames

;; ------------------------------------------------------------------------- ;;
;; Miscellaneous user created functions
(use-package misc-user-functions) 		    

(use-package fold-dwim
  :commands
  (fold-dwim-toggle   
   fold-dwim-show
   fold-dwim-hide
   fold-dwim-toggle-all
   fold-dwim-show-all
   fold-dwim-hide-all)

  :init
  ;; Toggle Folding on Whole Buffer
  (defvar fold-dwim-toggle-all-state
    nil
    "Saves the state of the folding of the document.")
  (make-variable-buffer-local 'fold-dwim-toggle-all-state)

  (defun fold-dwim-toggle-all ()
    "Toggles the document's folding.  This is not smart in that it does the
opposite of what it did last so it may be wrong if `fold-dwim-show-all' or
`fold-dwim-hide-all' was executed with out it knowing."
    (interactive)
    (if fold-dwim-toggle-all-state
	   (fold-dwim-show-all)
      (fold-dwim-hide-all))
    (setq fold-dwim-toggle-all-state
		(not fold-dwim-toggle-all-state)))
  
  :custom
  (hs-hide-comments-when-hiding-all	nil)
  (hs-allow-nesting					t)
  )

(use-package subr-x
  :commands string-trim)

(use-package tinylibm
  :commands ti::definteractive)

;; Expand Regions via S-SPC
(use-package expand-region)

;; (E)lectric (F)ile minor mode for mini-buffer
(use-package tinyef
  :hook (minibuffer-setup . turn-on-tinyef-mode))

;; Killing / Moving / Joining text in buffers
(use-package tinyeat
  :commands
  (tinyeat-kill-line
   tinyeat-kill-line-backward
   tinyeat-kill-buffer-lines-point-max
   tinyeat-kill-buffer-lines-point-min
   tinyeat-forward-preserve
   tinyeat-backward-preserve
   tinyeat-delete-paragraph
   tinyeat-zap-line
   tinyeat-join-lines))

;; ------------------------------------------------------------------------- ;;
;; Spelling / Auto Complete / Snippets / Web Search
;; ------------------------------------------------------------------------- ;;
;; Spell Check
(use-package ispell
  :commands ispell-mode
  :init
  ;; Add to path if not already there   
  (add-to-list 'exec-path		"C:/ProgramData/Aspell/bin")
  
  :custom
  (ispell-program-name			"aspell.exe")
  (ispell-list-command			"list")
  (ispell-extra-args			'("--sug-mode=fast")))

(use-package flyspell
  :commands (flyspell-prog-mode flyspell-mode)
  :hook (text-mode . flyspell-mode))

;; Auto Complete - Time: 9%
(use-package auto-complete-config
  :config
  (ac-config-default)

  (setq-default
   ac-use-quick-help					nil
   ac-quick-help-delay					5
   ac-show-menu-immediately-on-auto-complete	nil
   ac-ignore-case						nil
   ac-auto-show-menu					nil
   ac-auto-start						2)

  (ac-flyspell-workaround)				; :after flyspell
  
  (delete 'font-lock-string-face ac-disable-faces)

  ;; (defun ac-common-setup ()
  ;;   (setq ac-sources (append ac-sources '(ac-source-yasnippet
  ;; 								ac-source-filename))))

  (defun ac-prefix-valid-file ()
    "Existed (or to be existed) file prefix."
    (let* ((line-beg (line-beginning-position))
		 (end (point))
		 (start (or (let ((point (re-search-backward
							 ;; Added additional characters
							 "[\"\(\[`<>'= \t\r\n]"
							 line-beg t)))
				    (if point (1+ point)))
				  line-beg))
		 (file (buffer-substring start end)))
	 (if (and file (or (string-match "^/" file)
				    (and (setq file (and (string-match "^[^/]*/" file)
									(match-string 0 file)))
					    (file-directory-p file))))
		(unless (ac-windows-remote-file-p file)
		  start))))

  ;; ---------- Keys ---------- ;;
  (define-keys		ac-completing-map
    (kbd "<tab>")		'ac-next
    [(return)]			'ac-complete)
  )

;; Icicles - Time: 54%
(use-package icicles
  :custom
  (icicle-show-Completions-help-flag			nil)
  (icicle-candidate-width-factor				100)
  (icicle-Completions-display-min-input-chars	2)
  (icicle-change-region-background-flag			t)
  (icicle-region-background					"black")
  
  :config
  (icy-mode t))

;; Snippets
(use-package yasnippet
  :after keybinding
  :config
  (yas-global-mode 1)
  (define-keys			yas-keymap
    (kbd "<delete>")		(yas-filtered-definition
						 yas-maybe-clear-field)
    (kbd "<C-delete>")		(yas-filtered-definition
						 yas-maybe-skip-and-clear-field)))

;; Tool Tip Formatting
(use-package my-pos-tip
  :commands pos-tip-show
  :config
  (set-face-attribute 'popup-tip-face nil
				  :font		"envy code r"
				  :height		90
				  :background	"#00222c"
				  :foreground	"light gray"))			

;; Provide web searching functionality
(use-package websearch)

;; ------------------------------------------------------------------------- ;;
;; Buffer Stuff
;; ------------------------------------------------------------------------- ;;
;; Cleanup 
;;	Removes all *--* buffers after 5s of idle and others every 1d
;;	Active process / unsaved work / etc is safe
(use-package midnight
  :config
  (setq
   clean-buffer-list-kill-regexps		'("^\\*.*\\*$")
   clean-buffer-list-delay-special		0
   clean-buffer-list-delay-general		1
   ;; Create a timer
   timer-midnight
   (run-with-idle-timer
    10 t							; idle for 10s every time (t)
    '(lambda ()
       "Wrapper to for `clean-buffer-list' to remove messages.
		Use (cancel-timer timer-midnight) to cancel."
       (cl-letf
		 (((symbol-function 'message) #'format))
	    (clean-buffer-list))))))

;; Buffer Window
(use-package bs
  :commands bs-show
  :custom
  (bs-attributes-list   
   '((""		1 1 left bs--get-marked-string)
     ("M"	1 1 left bs--get-modified-string)
     ("R"	2 2 left bs--get-readonly-string)
     ("Buffer"	bs--get-name-length 10 left bs--get-name)
     (""		2 2 left "  ")
     ("Size"	8 8 right bs--get-size-string)
     (""		2 2 left "  ")
     ("Mode"	16 16 middle bs--get-mode-name)
     (""		2 2 left "  "))))

;; DIR Window
(use-package dired+
  :commands dired
  :config
  (toggle-diredp-find-file-reuse-dir 1)
  (diredp-make-find-file-keys-reuse-dirs))


;; -----------------------------------------------------------------------------
;; Key-Binding
;; -----------------------------------------------------------------------------
(use-package keybinding)			; General Key-binding Setup - 9%
(use-package mouse3)			; Additional Mouse Button functions


;; ----------------------------------------------------------------------------
;; Modes
;; ----------------------------------------------------------------------------
(use-package lisp-setup
  :hook ((emacs-lisp-mode		. my-lisp-mode-hook)
	    (lisp-mode				. my-lisp-mode-hook)
	    (lisp-interaction-mode	. my-lisp-mode-hook))
  :init
  (message ">>> :init lisp-setup")

  :config
  (message ">>> :config lisp-setup"))

(use-package comint
  :commands comint-mode
  :config
  (define-keys		comint-mode-map
    [C-down]			'comint-next-prompt
    [C-up]			'comint-previous-prompt
    ;; These are nice (forget about previous/next-input)
    [down]			'comint-next-matching-input-from-input
    [up]				'comint-previous-matching-input-from-input
    [S-C-up]			'previous-line
    [S-C-down]			'next-line
    
    ;; ---------- Help ----------
    (kbd "C-h f")   	'man-at-point 
    [(f1)]			'(lambda ()
					   (interactive)
					   (google-query-at-point t "bash "))
    (kbd "C-h w")   	'(lambda ()
					   (interactive)
					   (google-query-at-point nil "bash "))

    ;; ---------- Frame Switching ----------
    [(f12)]			'switch-frame-previous
    [S-f12]              'shell-new)
  )

(use-package r-setup
  :init (require 'ess-site)
  :hook ((ess-mode				. my-r-mode-hook)
	    (inferior-ess-mode		. my-inferior-r-mode-hook)
	    (ess-help-mode			. (lambda () (font-lock-mode t)))))

(use-package sql-setup
  :hook ((sql-mode				. my-sql-mode-hook)
	    (sql-interactive-mode	. my-sql-interactive-mode-hook))
  )

(use-package python-setup
  :hook ((python-mode			. my-python-mode-hook)
	    (inferior-python-mode	. my-inferior-python-mode-hook))
  )

(use-package markdown-mode
  :commands markdown-mode
  :init
  (message ">>> markdown-mode: init run")

  :hook
  (markdown-mode-hook
   . (lambda ()
	  (message ">>> markdown-mode: hook run")
	  (auto-complete-mode)
	  (auto-fill-mode 0)
	  (setq
	   tab-width				2
	   fill-column				99999999
	   electric-pair-pairs		'((?`. ?`)))

	  (define-keys
	    markdown-mode-map
	    ;; ---------- Evaluation ----------
	    [(shift return)]		markdown-preview

	    (kbd "<tab>")			tab-to-tab-stop
	    ;; ---------- Styles ----------
	    (kbd "C-i")	          markdown-insert-italic
	    (kbd "C-b")	          markdown-insert-bold 
	    (kbd "C-l")	          markdown-insert-link 
	    (kbd "C-j")	          markdown-insert-code 
	    
	    ;; [(f3)]				markdown-shifttab
	    
	    ;; ---------- Hide-Show ----------
	    [(f3)]				markdown-hide-subtree
	    [(shift f3)]			markdown-show-subtree
	    
	    ;; ---------- Help ----------
	    "\C-hf"		(lambda ()
					  (interactive)
					  (browse-url
					   (concat "https://github.com/adam-p/markdown-here"
							 "/wiki/Markdown-Cheatsheet")))
	    [(S-f1)]	     (lambda ()
					  (interactive)
					  (browse-url
					   (concat "https://github.com/adam-p/markdown-here"
							 "/wiki/Markdown-Cheatsheet")))
	    (kbd "C-h w")	(lambda ()
					  (interactive)
					  (google-query-at-point nil "markdown ")))
	  ))

  :custom
  (markdown-command			"multimarkdown")

  :config
  (message ">>> markdown-mode: config run")
  (message "%s" markdown-command)

  )

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
		    ("\C-m"			. newline-and-indent)))

(use-package shell-setup
  :mode ("\\.bat\\'"			. ntcmd-mode)
  :hook ((ntcmd-mode			. my-bat-mode-hook)
	    (shell-mode			. my-shell-mode-hook)))

(use-package powershell-setup
  :hook ((powershell-launch		. my-powershell-hook)
	    (powershell-mode		. my-powershell-mode-hook)))

;; (use-package json-mode :ensure t
;;   :mode "\\.json\\'"
;;   )

;; (use-package css-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.scss\\'" "\\.sass\\'")
;;   )

;; (require 'web-setup nil t)
;; (require 'latex-setup nil t)

;; https://www.emacswiki.org/emacs/ElectricHelp
;; (require 'ehelp nil t)
;; (global-set-key "\C-h" 'ehelp-command)

(custom-set-variables
 '(ac-use-quick-help nil)
 
 ;; ESS
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
	(ess-R-fl-keyword:fun-defs . t)
	(ess-R-fl-keyword:keywords . t)
	(ess-R-fl-keyword:assign-ops . t)
	(ess-R-fl-keyword:constants . t)
	(ess-fl-keyword:fun-calls . t)
	(ess-fl-keyword:numbers . t)
	(ess-fl-keyword:operators)
	(ess-fl-keyword:delimiters)
	(ess-fl-keyword:= . t)
	(ess-R-fl-keyword:F&T . t)
	(ess-R-fl-keyword:%op% . t))))
 '(ess-ask-for-ess-directory		nil)	; Suppress ask for directory
 '(ess-default-style			(quote RStudio))
 '(ess-eval-visibly				nil)
 '(ess-help-kill-bogus-buffers	t)	; Kill silly buffers
 '(ess-help-own-frame			1)
 '(ess-history-file				nil)
 '(ess-keep-dump-files			nil)
 '(ess-r-args-electric-paren		nil)
 '(ess-r-args-noargsmsg			"No Args")
 '(ess-r-args-show-as			(quote tooltip)) ; R ARGS as tool tip
 '(ess-r-args-show-prefix		"")			  ; Remove ARG Prefix
 '(inferior-R-args
   "--no-restore-history --no-save")
 '(inferior-R-program-name
   "~\\R\\R-4.0.2\\bin\\x64\\Rterm.exe")
 
 ;; Python
 '(python-guess-indent			nil)
 '(python-indent				4)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset			4)
 '(python-shell-interpreter
   "C:/Users/hilbertjp2/AppData/Local/Continuum/anaconda3/Scripts/ipython.exe")

 ;; SQL
  '(sql-ms-program
    "C:/Program Files/Microsoft SQL Server/100/Tools/Binn/sqlcmd.exe")
  '(sql-oracle-program			"sqlplus")
  '(sql-oracle-scan-on			nil)
  '(sql-send-terminator			nil)	; since I don't put GO after
								; (CAUSE ISSUES IN SQLPLUS if non-nil)
  '(sql-ms-options
    '("-w" "2000"					; Max Column Width
	 "-y" "2000"					; Individual Char Width
	 "-s" "|"						; Column Separator
	 "-k"))
  '(sql-product				(quote oracle)) ; Default
  )

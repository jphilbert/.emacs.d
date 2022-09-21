;;; config-ui.el --- Emacs Config: UI optimizations and tweaks.

;;; Code:



(setq-default initial-major-mode				'emacs-lisp-mode)


(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(set-scroll-bar-mode nil)
(set-fringe-mode 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-line-numbers-mode -1)			; <-- use this

;; Substitute y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; use zenburn as the default theme
(load-theme 'zenburn t)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)
 




;; '(select-enable-clipboard t)
;; '(show-paren-mode t)


;; enable PRETTY-LAMBDA-MODE
(global-prettify-symbols-mode +1)





;; Set Default Size and Font
(set-face-attribute 'default nil
				:font "envy code r"
				:height 110		; 11pt
				:weight 'normal)

(require 'octicons)
(set-face-attribute 'octicons nil
				:font "github-octicons")


(setq zenburn-override-colors-alist
	 '(("zenburn-orange-1"   . "#D28E60")
	   ("zenburn-orange-2"   . "#C77138")))


;; This one takes exp arguments
(defun zenburn-color-hex (color)
  "Returns the HEX of COLOR defined in `zenburn-colors-alist'"  
  (cadr (assq color
		    (mapcar (lambda (cons)
				    (list (intern (car cons)) (cdr cons)))
				  (append zenburn-default-colors-alist
						zenburn-override-colors-alist)))))

;; This one takes strings arguments
(defun zenburn-color-hex (color)
  "Returns the HEX of COLOR defined in `zenburn-colors-alist'"  
  (cdr (assoc color
		    (append zenburn-default-colors-alist
				  zenburn-override-colors-alist))))


(defface minibuffer-face
  '((t (:background "gray10")))
  "Default face for the minibuffer (see `config-minibuffer-face').")

(defun config-minibuffer-face ()
  "Sets `minibuffer-face' to buffers.

Primarily verifies buffers are alive, are non-empty, and sets the face
to `minibuffer-face' for the following buffers:
    ' *Minibuf-0*'
    ' *Minibuf-1*'
    ' *Echo Area 0*'
    ' *Echo Area 1*'

For some reason the face may be reset upon exiting the minibuffer so
it is advised to add this to the `minibuffer-inactive-mode-hook'.

Taken from `solaire-mode.el' (see
https://github.com/hlissner/emacs-solaire-mode/blob/8af65fbdc50b25ed3214da949b8a484527c7cc14/solaire-mode.el#L310
)"
  (let ((spec
         (--mapcat (list (car it) (face-attribute 'minibuffer-face (car it)))
                   face-attribute-name-alist))
        (minibuffers
         '(" *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*")))

    (--each minibuffers
      (with-current-buffer (get-buffer-create it)
        (apply #'face-remap-set-base 'default spec)
        ;; Minibuffers must be non-empty for face to be applied whole line.
        (when (= (buffer-size) 0)
          (insert " "))
      ))))

(config-minibuffer-face)                ; set it now
(add-hook 'minibuffer-inactive-mode-hook #'config-minibuffer-face)

(defface completions-candidate-face
  '((t (:background "gray10")))
  "Default face for completions.")

(add-hook 'minibuffer-setup-hook
		  '(lambda () (buffer-face-set 'completions-candidate-face)))


;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(zenburn-with-color-variables
  (custom-set-faces
   `(vertico-current
	((t
	  (:foreground		,zenburn-fg
	   :weight		bold
	   :background		,zenburn-blue-4
	   :underline		nil))))

   `(completions-annotations
	((t
	  (:foreground		,zenburn-red-6))))))


(zenburn-with-color-variables
   `(lazy-highlight
	((t
	  (:foreground		,zenburn-orange-2
	   :weight		bold
	   :background		,zenburn-bg+05))))

   
   ;; ---------- Auto Complete ---------- ;;
   `(ac-candidate-face
	((t
	  (:height		100
	   :slant			normal
	   :weight		normal))))
   `(ac-selection-face
	((t
	  (:height		100
	   :slant			normal
	   :weight		normal))))
   `(ac-yasnippet-candidate-face
	((t
	  (:inherit		'ac-candidate-face
	   :foreground		,zenburn-red-6))))
   `(ac-yasnippet-selection-face
	((t
	  (:inherit		'ac-selection-face
	   :foreground		,zenburn-yellow-2))))

   ;; ---------- Comments ---------- ;;
   `(font-lock-comment-face
	((t
	  (:foreground		,zenburn-green-1))))
   `(font-lock-comment-delimiter-face
	((t
	  (:foreground		,zenburn-green-1))))

   ;; ---------- Relational Operators ---------- ;;
   `(font-lock-relation-operator-face
	((t
	  (:foreground		,zenburn-orange
	   :weight			bold))))

   ;; ---------- Numbers ---------- ;;
   `(font-lock-number-face 
	((t
	  (:foreground		,zenburn-blue-3))))
   )

;; TO-DO:
;;   - add modeline-posn to modeline
;;	- add button to process --> go to process buffer
;;	- make simpler modeline for process --> row - mode
;;		- add clear buffer

;; ------------------------------------------------------------------------- ;;
;; Mode Lines
;; ------------------------------------------------------------------------- ;;
(require 'modeline-posn)
(require 'powerline)

(defface mode-line-1
  '((t (:inherit		mode-line)))
  ""
  :group 'mode-line-faces)

(defface mode-line-2
  '((t (:inherit		mode-line)))
  ""
  :group 'mode-line-faces)

(defface mode-line-1-inactive
  '((t (:inherit		mode-line-inactive)))
  ""
  :group 'mode-line-faces)

(defface mode-line-2-inactive
  '((t (:inherit		mode-line-inactive)))
  ""
  :group 'mode-line-faces)

(defface mode-line-modified-face
  '((t (:inherit		mode-line
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-read-only-face
  '((t (:inherit		mode-line
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-column-warn-face
  '((t (:inherit		mode-line
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-mode-face
  '((t (:inherit		mode-line
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-mode-inactive-face
  '((t (:inherit		mode-line-inactive
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-process-face
  '((t (:inherit		mode-line
	   :background		nil)))
  ""
  :group 'mode-line-faces)

(defface mode-line-process-inactive-face
  '((t (:inherit		mode-line-inactive
	   :background		nil)))
  ""
  :group 'mode-line-faces)

;; ---------- Mode Line Faces ---------- ;;
(zenburn-with-color-variables
  (custom-set-faces
   `(mode-line
     ((t (:foreground		,zenburn-green+1
		:background		,zenburn-bg-2
		:height			80
		:box
		(:line-width		-1
		 :style			released-button)))))
   `(mode-line-1
	((t (:inherit			mode-line
		:background		,zenburn-bg-1))))
   `(mode-line-2
	((t (:inherit			mode-line
		:background		,zenburn-bg))))

   `(mode-line-inactive
	((t (:inherit			mode-line
		:foreground		,zenburn-green+3
		:background		,zenburn-bg-1))))
   `(mode-line-1-inactive
	((t (:inherit			mode-line-inactive
		:background		,zenburn-bg))))
   `(mode-line-2-inactive
	((t (:inherit			mode-line-inactive
		:background		,zenburn-bg+1))))

   `(mode-line-modified-face
	((t (:inherit			mode-line
		:foreground		,zenburn-red
		:background		nil
		:weight			bold
		:box
		(:line-width		2
		 :color			,zenburn-red)))))
   `(mode-line-read-only-face
	((t (:foreground		,zenburn-red-2
		))))
   
   `(mode-line-column-warn-face
	((t (:inherit			mode-line-position-face
		:inverse-video		t
		:weight			bold))))
   
   `(mode-line-mode-face
	((t (:inherit			mode-line-2
		:foreground		,zenburn-blue+1
		:background		nil		
		:weight			bold))))
   `(mode-line-mode-inactive-face
	((t (:inherit			mode-line-2-inactive
		:foreground		,zenburn-blue))))

   `(mode-line-process-face
	((t (:inherit			mode-line-2
		:foreground		,zenburn-yellow
		:background		nil		
		:weight			bold))))
   `(mode-line-process-inactive-face
	((t (:inherit			mode-line-2-inactive
		:foreground		,zenburn-yellow-1))))))

;; Mode Line - Region Section
(setq-default
 modeline-region 
 '(:eval
   (propertize
    ;; Text
    (if (or modelinepos-region-acting-on
		  (condition-case nil
			 (modelinepos-show-region-p)
		    (error nil)))
	   (condition-case nil
		  (let ((rows (if modelinepos-rect-p
					   ;; Rows (rectangle)
					   (count-lines (region-beginning) (region-end))
					 ;; Lines
					 (count-lines (mark t) (point))))
			   (chars (abs (- (mark t) (point))))
			   (cols (when modelinepos-rect-p        ; Columns (rectangle)
					 (if (fboundp 'rectangle--pos-cols) ; Emacs 25+
						(let ((rpc  (save-excursion
								    (rectangle--pos-cols (region-beginning)
													(region-end)))))
						  (abs (- (car rpc) (cdr rpc))))
					   (let ((start  (region-beginning))
						    (end    (region-end))
						    startcol endcol)
						(save-excursion
						  (goto-char start)
						  (setq startcol   (current-column))
						  (beginning-of-line)
						  (goto-char end)
						  (setq endcol  (current-column))
								; Ensure start column is the left one.
						  (when (< endcol startcol) 
						    (let ((col  startcol))
							 (setq startcol  endcol
								  endcol    col)))
						  (abs (- startcol endcol))))))))
		    (if modelinepos-rect-p
			   (format " %d cols, %d rows" cols rows)
			 (format " %d chars, %d lines" chars rows)))
		(error ""))
	 " ")
    'local-map		mode-line-column-line-number-mode-map
    'mouse-face		'mode-line-highlight
    'help-echo		"Buffer position, mouse-1: Line/col menu")))

;; Mode Line - Position Section
(setq-default
 modeline-position
 '(:eval
   (propertize
    ;; Format
    " (%c, %l) "
    ;; Face
    'face	(and
		 (>
		  (current-column)
		  80)
		 'mode-line-column-warn-face)    
    'local-map		mode-line-column-line-number-mode-map
    'mouse-face	'mode-line-highlight
    'help-echo		"Line and column, mouse-1: Line/col menu")))

;; Mode Line - Read Only Icon
(setq-default
 modeline-read-only
 '(:eval
   (propertize
    ;; Format
    (if buffer-read-only
    	   octicon-lock
    	 octicon-pencil)
    ;; Face
    'face			'octicons
    'local-map		'(keymap
				  (mode-line keymap
						   (mouse-1 . mode-line-toggle-read-only)))
    'mouse-face	'mode-line-highlight
    'help-echo		'mode-line-read-only-help-echo)))

;; Mode Line - Modified Icon
(setq-default
 modeline-modified
 '(:eval
   (propertize
    ;; Format
    (if (buffer-modified-p)
    	   octicon-alert
    	 "    ")
    ;; Face
    'face			'(octicons mode-line-read-only-face)
    'local-map		'(keymap
    				  (mode-line keymap
    						   (mouse-1 . save-buffer)))
    'mouse-face	'mode-line-highlight
    'help-echo		'(format
				  "Buffer is %smodified\nmouse-1: Save buffer"
				  (if (buffer-modified-p)
					 ""
				    "not ")))))


(setq powerline-height nil)
(setq powerline-text-scale-factor 0.9)

;; Mode Line
(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active
		  (powerline-selected-window-active))
		 (process
		  (get-buffer-process (current-buffer)))
		 ;; Faces
		 (mode-face
		  (if active 'mode-line-mode-face 'mode-line-mode-inactive-face))
		 (process-face
		  (if active 'mode-line-process-face
		    'mode-line-process-inactive-face))
		 (face-0
		  (if active 'mode-line 'mode-line-inactive))
		 (face-1
		  (if active 'mode-line-1 'mode-line-1-inactive))
		 (face-2
		  (if active 'mode-line-2 'mode-line-2-inactive))

		 (modeline-icon
		  (propertize
		   ;; Format
		   (cond
		    (process					octicon-terminal)
		    ((derived-mode-p 'text-mode)	octicon-file-text)
		    (t						octicon-file-code))
		   
		   ;; Face
		   'face			'octicons
		   'mouse-face		'mode-line-highlight
		   'help-echo
		   (cond
		    (process					"Terminal / Process")
		    ((derived-mode-p 'text-mode)	"Text File")
		    (t						"Code File"))
		   ))
		 
		 ;; Separators
		 (separator-left
		  (intern (format "powerline-%s-%s"
					   (powerline-current-separator)
					   (car powerline-default-separator-dir))))
		 (separator-right
		  (intern (format "powerline-%s-%s"
					   (powerline-current-separator)
					   (cdr powerline-default-separator-dir))))
		 
		 ;; ---------- Left Hand Side ---------- ;;
		 ;; - Read-Only
		 ;; - Modified
		 (lhs
		  (list
		   (powerline-raw	" " face-0)
		   (powerline-raw	modeline-icon face-0 'l)
		   (powerline-raw	" " face-0)
		   (powerline-raw	modeline-read-only face-0 'l)
		   (powerline-raw	" " face-0)
		   (powerline-raw	modeline-modified face-0 'l)
		   (powerline-raw	" " face-0)
		   (funcall separator-left face-0 face-1)
		   (powerline-narrow face-1 'l)
		   (powerline-raw	modeline-region face-1 'l)			    
		   (powerline-vc face-1)))

		 (lhs (if process
				(list
				 (powerline-raw	" " face-0)
				 (powerline-raw	modeline-icon face-0 'l)
				 (powerline-raw	"  " face-0)
				 (funcall separator-left face-0 face-1)
				 (powerline-narrow face-1 'l)
				 (powerline-raw	modeline-region face-1 'l)
				 (powerline-vc face-1))
			   lhs))
		 
		 ;; ---------- Center ---------- ;;
		 ;; - Major Mode
		 ;; - Process
		 (center
		  (list
		   (powerline-raw	" " face-1)
		   (funcall separator-left face-1 face-2)
		   ;; (when (and (boundp 'erc-track-minor-mode)
		   ;; 		    erc-track-minor-mode)
		   ;; 	(powerline-raw erc-modified-channels-object face-2 'l))
		   (powerline-raw	" " mode-face)
		   (powerline-major-mode mode-face 'l)
		   (powerline-raw	" " mode-face)
		   (powerline-process process-face)
		   ;; (powerline-minor-modes face-2 'l)
		   (powerline-raw	" " face-2)
		   (funcall separator-right face-2 face-1)))
		 
		 ;; ---------- Right Hand Side ---------- ;;
		 (rhs
		  (list
		   (powerline-raw	global-mode-string face-1 'r)
		   (powerline-raw	modeline-position face-1 'r)
		   ;; (powerline-raw "%4l" face-1 'r)
		   ;; (powerline-raw ":" face-1)
		   ;; (powerline-raw "%3c" face-1 'r)
		   (funcall separator-right face-1 face-0)
		   (powerline-raw	" " face-0)
		   (when powerline-display-buffer-size
			(powerline-buffer-size face-0 'r))
		   (powerline-raw	"/ " face-0)
		   (powerline-raw	"%3p" face-0 'r)
		   (when powerline-display-hud
			(powerline-hud face-2 face-1))
		   (powerline-fill	face-0 0)))

		 (rhs (if process
				(list
				 (powerline-raw	global-mode-string face-1 'r)
				 (powerline-raw	modeline-position face-1 'r)
				 (funcall separator-right face-1 face-0)
				 (powerline-raw	"      " face-0)
				 (powerline-fill	face-0 0))
			   rhs))
		 )
	 
	 (concat (powerline-render lhs)
		    (powerline-fill-center face-1 (/ (powerline-width center) 2.0))
		    (powerline-render center)
		    (powerline-fill face-1 (powerline-width rhs))
		    (powerline-render rhs))))))







;; ----------------------------------------------------------------------------
;; Additional Faces
;; ----------------------------------------------------------------------------
(make-face		'font-lock-number-face)
(make-face		'font-lock-relation-operator-face)

(set-face-attribute font-lock-function-name-face nil
				:weight 'bold)

;; ;; -------------------- R Faces --------------------
;; (make-face		'font-lock-ess-functions-face)
;; (set-face-attribute 'font-lock-ess-functions-face nil
;; 				 :foreground "DodgerBlue1")
;; ;; (set-face-attribute 'font-lock-ess-functions-face nil :weight 'bold)

;; (make-face		'font-lock-ess-dataframe-face)
;; (set-face-attribute 'font-lock-ess-dataframe-face nil
;; 				:foreground "khaki1"
;; 				:weight 'normal)

;; (make-face		'font-lock-ess-help-heading-2-face)
;; (set-face-attribute 'font-lock-ess-help-heading-2-face nil
;; 				:height 1.5)

;; (make-face		'font-lock-ess-help-heading-1-face)
;; (set-face-attribute 'font-lock-ess-help-heading-1-face nil
;; 				:height 2.0)


;; -------------------- Web Faces --------------------
(make-face		'web-mode-html-tag-face)
(set-face-attribute 'web-mode-html-tag-face nil
                    :foreground "gray60")

(make-face		'web-mode-html-attr-name-face)
(set-face-attribute 'web-mode-html-attr-name-face nil
                    :foreground "LightSteelBlue")

(make-face		'web-mode-current-element-highlight-face)
(set-face-attribute 'web-mode-current-element-highlight-face nil
                    :background "SteelBlue4")


;; -------------------- Java Script Faces -------------------- 
(make-face		'js2-function-param)
(set-face-attribute 'js2-function-param nil
				:foreground "green2")

(make-face		'js2-external-variable)
(set-face-attribute 'js2-external-variable nil
				:foreground "SandyBrown")


;; -----------------------------------------------------------------------------
;; Cursor Changes
;; -----------------------------------------------------------------------------
;; This is from Juri Linkov <juri@jurta.org> + Drew Adams, with read-only added.
(defcustom default-frame-cursor-type 'bar
  "*Default text cursor type for non-special frames. Valid options are 'box' 'hollow' 'bar' 'hbar'"
  :type 'symbol)

(defcustom default-frame-cursor-type-overwrite/read-only 'box
  "*Default text cursor type for overwrite mode or read-only buffer. Valid
options are 'box' 'hollow' 'bar' 'hbar'"
  :type 'symbol)

(defun change-cursor-on-overwrite/read-only ()
  "Set cursor type differently for overwrite mode and read-only buffer.  That
is, use one cursor type for overwrite mode and read-only buffers, and another
cursor type otherwise."
  (set-cursor-type
   (if (or buffer-read-only overwrite-mode)
	  default-frame-cursor-type-overwrite/read-only
	default-frame-cursor-type)))

(defun set-cursor-type (cursor-type)
  "Set the cursor type of the selected frame to CURSOR-TYPE.  When called
interactively, prompt for the type to use.  To get the frame's current cursor
type, use `frame-parameters'."
  (interactive
   (list (intern
		(completing-read "Cursor type: "
					  (mapcar 'list
							'("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters (selected-frame)
					  (list (cons 'cursor-type cursor-type))))

(add-hook 'post-command-hook 'change-cursor-on-overwrite/read-only)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face
   ((t (:height 100
	   :slant normal
	   :weight normal))))
 '(ac-selection-face
   ((t (:height 100
	   :slant normal
	   :weight normal))))
 '(ac-yasnippet-candidate-face
   ((t (:inherit 'ac-candidate-face
	   :foreground "#6C3333"))))
 '(ac-yasnippet-selection-face
   ((t (:inherit 'ac-selection-face
	   :foreground "#D0BF8F"))))
 '(completions-annotations
   ((t (:foreground "#6C3333"))))
 '(font-lock-comment-delimiter-face
   ((t (:foreground "#6F8F6F"))))
 '(font-lock-comment-face
   ((t (:foreground "#6F8F6F"))))
 '(font-lock-number-face
   ((t (:foreground "#5C888B"))) t)
 '(font-lock-relation-operator-face
   ((t (:foreground "#DFAF8F"
	   :weight bold))) t)
 '(lazy-highlight
   ((t (:foreground "#C77138"
	   :weight bold
	   :background "#494949"))))
 '(mode-line
   ((t (:foreground "#8FB28F"
	   :background "#000000"
	   :height 80
	   :box (:line-width -1
		    :style released-button)))))
 '(mode-line-1
   ((t (:inherit mode-line
	   :background "#2B2B2B"))))
 '(mode-line-1-inactive
   ((t (:inherit mode-line-inactive
	   :background "#3F3F3F"))))
 '(mode-line-2
   ((t (:inherit mode-line
	   :background "#3F3F3F"))))
 '(mode-line-2-inactive
   ((t (:inherit mode-line-inactive
	   :background "#4F4F4F"))))
 '(mode-line-column-warn-face
   ((t (:inherit mode-line-position-face
	   :inverse-video t
	   :weight bold))))
 '(mode-line-inactive
   ((t (:inherit mode-line
	   :foreground "#AFD8AF"
	   :background "#2B2B2B"))))
 '(mode-line-mode-face
   ((t (:inherit mode-line-2
	   :foreground "#94BFF3"
	   :background nil
	   :weight bold))))
 '(mode-line-mode-inactive-face
   ((t (:inherit mode-line-2-inactive
	   :foreground "#8CD0D3"))))
 '(mode-line-modified-face
   ((t (:inherit mode-line
	   :foreground "#CC9393"
	   :background nil
	   :weight bold
	   :box (:line-width 2
		    :color "#CC9393")))))
 '(mode-line-process-face
   ((t (:inherit mode-line-2	   
	   :foreground "#F0DFAF"	   
	   :background nil	   
	   :weight bold))))
 '(mode-line-process-inactive-face
   ((t (:inherit mode-line-2-inactive
	   :foreground "#E0CF9F"))))
 '(mode-line-read-only-face
   ((t (:foreground "#AC7373"))))
 '(vertico-current
   ((t (:foreground "#DCDCCC"
	   :weight bold
	   :background "#4C7073"
	   :underline nil)))))




(provide 'config-ui)
;;; CONFIG-UI.EL ends here

;; -----------------------------------------------------------------------------
;; AESTHETICS.EL --- My General Buffer Aesthetics
;; -----------------------------------------------------------------------------
;; Filename:		AESTETICS.EL
;; Description:		My General Buffer Aesthetics
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2012-02-13 19:54:33
;; Version:		0.1
;; Last-Updated:	2014-09-04 
;; Compatibility:	GNU Emacs 23.2.1
;;
;; Features that might be required by this library:
;;	- Font: Envy Code R (11pt)
;;	- color-theme

;; !!!This file is NOT part of GNU Emacs!!! 

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; My General Buffer Aesthetics

;; Latest Windows Font is Envy Code R (11pt) from:
;;      http://damieng.com/blog/2008/05/26/envy-code-r-preview-7-coding-font-released 
;;

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; Thanks to Amit Patel (http://amitp.blogspot.com/2011/08/
;; emacs-custom-mode-line.html) and Dirk-Jan C. Binnema
;; (http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html) for the
;; tips on mode-line modifications.

;; Thanks to Juri Linkov <juri@jurta.org> + Drew Adams for the cursor changing
;;  code. 

;; -----------------------------------------------------------------------------
;; General Settings
;; -----------------------------------------------------------------------------
(provide 'aesthetics)

;; Set Default Size and Font
(set-face-attribute 'default nil
				:font "envy code r"
				:height 110		; 11pt
				:weight 'normal)

(setq custom-theme-directory
	 (expand-file-name (concat user-emacs-directory "themes/")))
(dolist (f (directory-files custom-theme-directory))
  (if (and (not (or (equal f ".") (equal f "..")))
		 (file-directory-p (concat custom-theme-directory f)))
	 (add-to-list 'custom-theme-load-path (concat custom-theme-directory f))))

(load-theme 'zenburn t)

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

;; (color-theme-initialize)
;; (color-theme-dark-blue2)

(zenburn-with-color-variables
  (custom-set-faces
   `(lazy-highlight
	((t
	  (:foreground		,zenburn-orange-2
	   :weight		bold
	   :background		,zenburn-bg+05))))

   ;; ---------- Mode Line Faces ---------- ;;
   `(mode-line
     ((,class
	  (:foreground		,zenburn-green+1
	   :background		,zenburn-bg-1
	   :height		80
	   :box
	   (:line-width	-1
	    :style		released-button)))
      (t
	  :inverse-video t)))
   `(mode-line-inactive
	((t
	  (:foreground		,zenburn-green-2
	   :background		,zenburn-bg+05
	   :height		80
	   :box
	   (:line-width	-1
	    :style		released-button)))))
   `(mode-line-modified-face
	((t 
	  (:foreground		,zenburn-red
	   :background		nil
	   :height		80
	   :weight		bold
	   :box
	   (:line-width	2
	    :color		,zenburn-red)))))
   `(mode-line-read-only-face
	((,class
	  (:foreground		,zenburn-red-2
	   :box
	   (:line-width	2
	    :color		,zenburn-red-2)))))
   `(mode-line-mode-face
	((,class
	  (:foreground		,zenburn-blue+1
	   :weight		bold))))
   `(mode-line-80col-face
	((,class
	  (:inherit		'mode-line-position-face
	   :inverse-video	t
	   :weight		bold))))
   `(mode-line-process-face
	((,class
	  (:foreground		,zenburn-yellow))))
   
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
  ))


;; TO-DO:
;;   - add modeline-posn to modeline
;;	- add button to process --> go to process buffer
;;	- make simpler modeline for process --> row - mode

;; (setq-default
;;  mode-line-format
;;  (list "%e"
;; 	  'mode-line-client
;; 	  'mode-line-modified
;; 	  'mode-line-remote
;; 	  "   "
;; 	  'mode-line-position
;; 	  "  "
;; 	  'mode-line-modes
;;    ;; 'mode-line-misc-info
;;    ;; 'mode-line-end-spaces
;;    )
;;  )

;; (require 'modeline-posn)
(column-number-mode 1)
(size-indication-mode 1)

;; --------------------------------------------------------------------------
;; Mode-Line Setup
;; --------------------------------------------------------------------------
(setq-default
 mode-line-format
 '(;; Position, including warning for 80 columns (0-8)
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (> (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ;; [+5] (9-13) 
   "     "
   
   ;; read-only or modified status (14-17)
   (:eval
    (cond (buffer-read-only
           (propertize " XX " 'face 'mode-line-read-only-face))		
          ((buffer-modified-p)
           (propertize " !! " 'face 'mode-line-modified-face))
          (t "    ")))

   ;; [+5] (18-22)
   "     "   

   ;; Percent / Size
   (:propertize "%p / %I" face mode-line-position-face) 

   ;; [+15]
   "               "
   
   ;; (:propertize "%b" face mode-line-filename-face)
   
   ;; Major Mode
   "- "
   (:propertize mode-name
                face mode-line-mode-face)
   " -"

   ;; [+15]
   "               "
   
   ;; (:eval (propertize (format-mode-line minor-mode-alist)
   ;;                    'face 'mode-line-minor-mode-face))
   
   ;; Process
   (:propertize mode-line-process
                face mode-line-process-face)
   ))

(make-face		'mode-line-read-only-face)
(make-face		'mode-line-modified-face)
(make-face		'mode-line-position-face)
(make-face		'mode-line-mode-face)
(make-face		'mode-line-process-face)
(make-face		'mode-line-80col-face)




;; -----------------------------------------------------------------------------
;; Additional Faces
;; -----------------------------------------------------------------------------
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


;;; AESTHETICS.EL ends here


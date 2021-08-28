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

(set-face-attribute 'default nil
				:font "envy code r"
				:height 110
				;; :underline nil
				:weight 'normal
				;; :slant 'normal
				;; :overline nil
				:background "#233b5a")

(color-theme-initialize)
(color-theme-dark-blue2)

;; --------------------------------------------------------------------------
;; Mode-Line Setup
;; --------------------------------------------------------------------------
(setq-default
 mode-line-format
 '(;; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   
   mode-line-client                     ; emacsclient [default -- keep?]
   "\t"
   
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ;; (mode-line-process
          ;;  (propertize " -- " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " !! " 'face 'mode-line-modified-face))
          (t "    ")))
   "  "
   
   (:propertize "%p / %I" face mode-line-position-face) ; Percent / Size
   "\t\t"
   
   ;; (:propertize "%b" face mode-line-filename-face)
   
   ;; Major Mode
   "- "
   (:propertize mode-name
                face mode-line-mode-face)
   " -\t"
   ;; (:eval (propertize (format-mode-line minor-mode-alist)
   ;;                    'face 'mode-line-minor-mode-face))
   
   ;; Process
   (:propertize mode-line-process
                face mode-line-process-face)
   ))

;; Extra mode line faces
(set-face-attribute 'mode-line nil
                    :foreground "gray60" :background "gray20"
                    :inverse-video nil
                    :height 80
                    :box '(:line-width 2 :color "grey15" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray30"
                    :inverse-video nil
                    :height 80
                    :box '(:line-width 2 :color "gray25" :style nil))

(make-face		'mode-line-read-only-face)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "DodgerBlue3"
                    :box '(:line-width 2 :color "DodgerBlue3"))

(make-face		'mode-line-modified-face)
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "firebrick1"
                    :background "OldLace"
                    :weight 'bold
                    :box '(:line-width 2 :color "firebrick1"))

(make-face		'mode-line-position-face)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    ;; :family "Menlo"
                    :height 80)

(make-face		'mode-line-mode-face)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "SteelBlue1"
                    :weight 'bold)

(make-face		'mode-line-process-face)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "PaleGreen3")

(make-face		'mode-line-80col-face)
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "firebrick1"
                    :background "grey20")




;; -----------------------------------------------------------------------------
;; Additional Faces
;; -----------------------------------------------------------------------------
(make-face		'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil
				:foreground "DarkSlateGray3")

(make-face		'font-lock-relation-operator-face)
(set-face-attribute 'font-lock-relation-operator-face nil
				:foreground "salmon1"
				:weight 'bold)

(set-face-attribute font-lock-function-name-face nil
				:weight 'bold)

;; -------------------- R Faces --------------------
(make-face		'font-lock-ess-functions-face)
(set-face-attribute 'font-lock-ess-functions-face nil
				 :foreground "DodgerBlue1")
;; (set-face-attribute 'font-lock-ess-functions-face nil :weight 'bold)

(make-face		'font-lock-ess-dataframe-face)
(set-face-attribute 'font-lock-ess-dataframe-face nil
				:foreground "khaki1"
				:weight 'normal)

(make-face		'font-lock-ess-help-heading-2-face)
(set-face-attribute 'font-lock-ess-help-heading-2-face nil
				:height 1.5)

(make-face		'font-lock-ess-help-heading-1-face)
(set-face-attribute 'font-lock-ess-help-heading-1-face nil
				:height 2.0)


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


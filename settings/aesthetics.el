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
;;      http://damieng.com/blog/2008/05/26
;;              /envy-code-r-preview-7-coding-font-released
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
(add-to-list 'pretty-lambda-auto-modes 'python-mode)
(pretty-lambda-for-modes)		; Lambda mode for all




;; Set the Font and Color
(set-face-attribute             'default
                                nil
                                :font "envy code r-13")
(set-background-color           "snow")

(color-theme-initialize)
(color-theme-dark-blue2)

(fringe-mode                    0)      ; Removes fringes
(global-visual-line-mode        t)      ; Word Wrapping
(global-font-lock-mode          t)      ; Syntax Coloring
(delete-selection-mode          t)      ; Entry deletes marked text
(show-paren-mode                t)      ; Highlight pairs
(mouse-avoidance-mode           'jump)  ; Moves cursor out of way
(tool-bar-mode                  0)      ; No Tool Bar
(menu-bar-mode                  0)      ; No Menu Bar
(toggle-scroll-bar              nil)    ; no scroll bars



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
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

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
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "DodgerBlue3"
                    :box '(:line-width 2 :color "DodgerBlue3"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "firebrick1"
                    :background "OldLace"
                    :weight 'bold
                    :box '(:line-width 2 :color "firebrick1"))
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    ;; :family "Menlo"
                    :height 80)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "SteelBlue1"
                    :weight 'bold)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "PaleGreen3")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "firebrick1"
                    :background "grey20")



;; --------------------------------------------------------------------------
;; Auto-Complete
;; --------------------------------------------------------------------------
(set-face-attribute 'ac-yasnippet-candidate-face
		    nil
		    :slant 'italic
		    :font "envy code r-11"
		    :background "#00222c"
		    :foreground "light gray")
(set-face-attribute 'ac-yasnippet-selection-face
		    nil
		    :slant 'italic
		    :font "envy code r-11"
		    :background "SteelBlue4"
		    :foreground "white")

(set-face-attribute 'ac-candidate-face
		    nil
		    :font "envy code r-11"
		    :background "#00222c"
		    :foreground "light gray")
(set-face-attribute 'ac-selection-face
		    nil
		    :font "envy code r-11"
		    :background "SteelBlue4"
		    :foreground "white")
(set-face-attribute 'popup-tip-face
		    nil
		    :font "envy code r-11"
		    :height 90
		    :background "#00222c"
		    :foreground "light gray")

(copy-face 'ac-candidate-face 'ac-yasnippet-candidate-face)
(set-face-attribute 'ac-yasnippet-candidate-face
		    nil
		    :foreground "sandybrown")
(set-face-attribute 'popup-summary-face
		    nil
		    :font "envy code r-11")



;; -----------------------------------------------------------------------------
;; Additional Faces
;; -----------------------------------------------------------------------------
(make-face 'font-lock-number-face)
(set-face-foreground 'font-lock-number-face "DarkSlateGray3")

(make-face 'font-lock-relation-operator-face)
(set-face-foreground 'font-lock-relation-operator-face "salmon1")
(set-face-attribute 'font-lock-relation-operator-face nil :weight 'bold)

(set-face-attribute font-lock-function-name-face nil :weight 'bold)

;; -------------------- R Faces --------------------
(make-face 'font-lock-ess-functions-face)
(set-face-foreground 'font-lock-ess-functions-face "DodgerBlue1")
;; (set-face-attribute 'font-lock-ess-functions-face nil :weight 'bold)

(make-face 'font-lock-ess-dataframe-face)
(set-face-foreground 'font-lock-ess-dataframe-face "khaki1")
(set-face-attribute 'font-lock-ess-dataframe-face nil :weight 'normal)

(make-face 'font-lock-ess-help-heading-2-face)
(set-face-attribute 'font-lock-ess-help-heading-2-face nil :height 1.5)

(make-face 'font-lock-ess-help-heading-1-face)
(set-face-attribute 'font-lock-ess-help-heading-1-face nil :height 2.0)


;; -------------------- Web Faces --------------------
(set-face-attribute 'web-mode-html-tag-face nil
                    :foreground "gray60")

(set-face-attribute 'web-mode-html-attr-name-face nil
                    :foreground "LightSteelBlue")

(set-face-attribute 'web-mode-current-element-highlight-face nil
                    :background "SteelBlue4")


;; -------------------- Java Script Faces -------------------- 
(set-face-foreground 'js2-function-param "green2")
(set-face-foreground 'js2-external-variable "SandyBrown")


;; -----------------------------------------------------------------------------
;; Special Mode Keywords
;; -----------------------------------------------------------------------------
;; -------------------- SQL --------------------
(font-lock-add-keywords
 'sql-mode
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("[!\\^<>]=\\|[<>]\\|\\(\\b\\(all\\|and\\|any\\|between\\|
exists\\|in\\|like\\|not\\|or\\|some\\)\\b\\)"
     .
     'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'sql-interactive-mode
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)))


;; -------------------- R --------------------
(font-lock-add-keywords
 'ess-mode
 '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*("
    1                               ; Signifies which group
    'font-lock-ess-functions-face)
   ("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*\\["
    1                               ; Signifies which group
    'font-lock-ess-dataframe-face)
   ("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("\\([<=>]=\\|[!<>&|]\\)[^-]"
    1
    'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'inferior-ess-mode
 '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*("
    1                               ; Signifies which group
    'font-lock-ess-functions-face)
   ("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*\\["
    1                               ; Signifies which group
    'font-lock-ess-dataframe-face)
   ("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)))

(font-lock-add-keywords
 'ess-help-mode
 '(("\\(?:Description\\|Usage\\|Arguments\\|Details\\|Value\\|S4 methods\\|References\\|See Also\\|Examples\\):"
    .
    'font-lock-ess-help-heading-2-face)))


;; -------------------- Python --------------------
(font-lock-add-keywords
 'python-mode        
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("[!<=>]=\\|[<>]\\|\\(\\b\\(and\\|or\\|not\\|in\\|is\\)\\b\\)"
     .
     'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'inferior-python-mode
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)))


;; -------------------- LISP --------------------
(font-lock-add-keywords
 'emacs-lisp-mode        
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("[!<>]=\\|[<>]\\|\\(\\b\\(and\\|or\\|not\\)\\b\\)"
     .
     'font-lock-relation-operator-face)))

;; -------------------- Java Script --------------------
(font-lock-add-keywords
 'js2-mode
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("\\([<=>]=+\\|[!<>&|]\\)"
    .
    'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'js-mode
 '(("\\<[0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("\\([<=>]=+\\|[!<>&|]\\)"
    .
    'font-lock-relation-operator-face)))


;; -----------------------------------------------------------------------------
;; Cursor Changes
;; -----------------------------------------------------------------------------
;; This is from Juri Linkov <juri@jurta.org> + Drew Adams, with read-only added.
(defun change-cursor-on-overwrite/read-only ()
  "Set cursor type differently for overwrite mode and read-only buffer.
That is, use one cursor type for overwrite mode and read-only buffers,
and another cursor type otherwise."
  (set-cursor-type (if (or buffer-read-only overwrite-mode)
                       default-frame-cursor-type-overwrite/read-only
                     default-frame-cursor-type)))

(defun set-cursor-type (cursor-type)
  "Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'."
  (interactive
   (list (intern (completing-read "Cursor type: "
                                  (mapcar 'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters (selected-frame) (list (cons 'cursor-type cursor-type))))

(defcustom default-frame-cursor-type 'bar
  "*Default text cursor type for non-special frames. Valid
options are 'box' 'hollow' 'bar' 'hbar'"
  :type 'symbol)

(defcustom default-frame-cursor-type-overwrite/read-only 'box
  "*Default text cursor type for overwrite mode or read-only
buffer. Valid options are 'box' 'hollow' 'bar' 'hbar'"
  :type 'symbol)

(add-hook 'post-command-hook 'change-cursor-on-overwrite/read-only)

(provide 'aesthetics)

;;; AESTHETICS.EL ends here


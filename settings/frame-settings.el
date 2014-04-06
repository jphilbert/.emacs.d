;; -----------------------------------------------------------------------------
;; FRAME-SETTINGS.EL --- Custom Multi-Frame Setup
;; -----------------------------------------------------------------------------
;; Filename:		FRAME-SETTINGS.EL
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2013-07-23 08:42:02
;; Version:		2.0
;; Last-Updated:	2014-04-06
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 24.3
;;
;; Features that might be required by this library:
;; <NONE>

;; ---------- !!!This file is NOT part of GNU Emacs!!! ----------

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; Sets ups Emacs for multiple frames.  This works in two major ways:
;;	1) Forces Emacs to open new files (in and out of Emacs) in a new frame
;;	2) Sets up `display-buffer-alist' for showing intermediate buffers on a per
;;	   buffer basis
;; We due this since we typically do not want 'every' buffer to show in a new
;; frame (such as auto-complete, etc).  Also `display-buffer-alist' allows
;; special placement of frames and such (e.g. resize, auto-lower, switch back,
;; etc.)
;;
;; There are various custom variables that set the default frame size and
;; height.
;; 

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put FRAME-SETTINGS.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'frame-settings)
;;	

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------
(defvar Frame-Terminal-Top 5
  "Default terminal window top position")
(defvar Frame-Terminal-Left (/ (x-display-pixel-width) 2)
  "Default terminal window left position")

(defvar Frame-Default-Height 45
  "The default window (frame) height in characters")

(defvar Frame-Default-Width 81
  "The default window (frame) width in characters")

(defvar Frame-Default-Top 5
  "The default window (frame) top in characters")

(defvar Frame-Default-Left 5
  "The default window (frame) left in characters")

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2013-07-23
;;      * First released.
;; 2014-04-06
;;	* Updated to Emacs 24
;;		- Now uses `display-buffer-alist'
;;		- Special frame handling (such as resizing) is built in
;;	* Moved most of setup handling to here (previously in multi-window.el)
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
(require 'fit-frame)
(require 'switch-frame)
(require 'frame-functions-misc)


;; -----------------------------------------------------------------------------
;; Setup
;; -----------------------------------------------------------------------------
;; Sets title bar as "<buffer>"
;;      Note: Emacs sets it to this if there are multiple frames anyways
(setq frame-title-format "%b")

;; Change file opening to popup in other frame
(defalias 'find-file 'find-file-other-frame)

;; When opening a file from outside emacs the server processes the request, this
;;  handles creating a new frame for the buffer
(add-hook 'server-visit-hook		'new-frame)

;; Delete all frames showing buffer after kill-buffer
(add-hook 'kill-buffer-hook		'delete-windows-on)

;; Make the GUI close (X) button act like kill-buffer
(defadvice handle-delete-frame (around delete-frame-after-kill activate)
  "Map (X) button to kill-buffer"
  (let ((frame   (posn-window (event-start event))))
    (kill-buffer-or-emacs (window-buffer (get-largest-window frame)))))

;; Fit Frame Setup
(setq-default
 fit-frame-max-height-percent		70
 fit-frame-max-width			Frame-Default-Width)

;; -----------------------------------------------------------------------------
;; Frame Settings
;; -----------------------------------------------------------------------------

;; Common Display Properties
;; -------------------------
;;      width 
;;      height
;;      mouse-color 
;;      cursor-color
;;      menu-bar-lines  
;;      foreground-color
;;      background-color
;;      top
;;      left
;;      unsplittable . nil              ; Very important
;;      user-position                                   
;;      vertical-scroll-bars
;;	(auto-raise . t)
;;	(auto-lower . t)		; Nice

;; For pop-up-frame-parameters see:
;; Manuals/elisp-manual-20-2.5/html_node/elisp_434.html
;;
;; See Also:
;;	http://www.gnu.org/s/emacs/manual/html_node/elisp/
;;	Window-Frame-Parameters.html  

;; -------------------- Default Frame ------------------
(add-to-list 'default-frame-alist 
		   `(height . ,Frame-Default-Height))
(add-to-list 'default-frame-alist
		   `(width . ,Frame-Default-Width))
(add-to-list 'default-frame-alist
		   `(top . ,Frame-Default-Top))
(add-to-list 'default-frame-alist
		   `(left . ,Frame-Default-Left))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; -------------------- Help Frame ---------------------
(add-to-list 'display-buffer-alist
		   `("\\*.*help.*\\*"
			(lambda (b a)
			  (let ((return-window
				    (cond
					((display-buffer-reuse-window b a))
					((display-buffer-pop-up-frame b a)))))
			    (fit-frame (get-frame b))   ; Fit Buffer
			    (setq mode-line-format nil) ; Remove Mode Line
			    return-window))	     
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . 15)
			  (width . ,Frame-Default-Width)
			  (top . 10)
			  (left . ,(/ (x-display-pixel-width) 2))))))

;; -------------------- Python Help Frame ---------------------
;; TO DO: Auto Resize 
(add-to-list 'display-buffer-alist
		   `("\\*.*jedi.*\\*"
			(lambda (b a)
			  (let ((return-window
				    (cond
					((display-buffer-reuse-window b a))
					((display-buffer-pop-up-frame b a)))))
			    (fit-frame (get-frame b))   ; Fit Buffer
			    (setq mode-line-format nil) ; Remove Mode Line
			    return-window))	     
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . 15)
			  (width . ,Frame-Default-Width)
			  (top . 10)
			  (left . ,(/ (x-display-pixel-width) 2))))))

;; -------------------- Occur Frame --------------------
(add-to-list 'display-buffer-alist
		   `("\\*Occur.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . 35)
			  (width . ,Frame-Default-Width)
			  (top . 10)
			  (left . ,(+ (/ (x-display-pixel-width) 2) 20))))))

;; --------------- Shell / Power Shell Frame -----------
(add-to-list 'display-buffer-alist
		   `("\\*shell.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 0 Frame-Terminal-Top))
			  (left . ,(+ 0 Frame-Terminal-Left))))))

(add-to-list 'display-buffer-alist
		   `("\\*PowerShell.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 10 Frame-Terminal-Top))
			  (left . ,(+ 10 Frame-Terminal-Left))))))

;; -------------------- R Frame ---------------------
(add-to-list 'display-buffer-alist
		   `("\\*R.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 20 Frame-Terminal-Top))
			  (left . ,(+ 20 Frame-Terminal-Left))))))

;; -------------------- Message Frame ---------------------
(add-to-list 'display-buffer-alist
		   `("\\*Messages\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 50 Frame-Terminal-Top))
			  (left . ,(+ 50 Frame-Terminal-Left))))))

;; -------------------- Backtrace Frame --------------------
(add-to-list 'display-buffer-alist
		   `("\\*Backtrace\\*"
			(lambda (b a)
			  (let ((return-window
				    (cond
					((display-buffer-reuse-window b a))
					((display-buffer-pop-up-frame b a)))))
			    (fit-frame (get-frame b))   ; Fit Buffer
			    (setq mode-line-format nil) ; Remove Mode Line
			    return-window))	  
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (auto-lower . t)
			  (minibuffer . nil)
			  ;; TODO: Remove Status Line
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . 15)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 60 Frame-Terminal-Top))
			  (left . ,(+ 60 Frame-Terminal-Left))))))

;; -------------------- Python Frame ---------------------
(add-to-list 'display-buffer-alist
		   `("\\*Python.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 40 Frame-Terminal-Top))
			  (left . ,(+ 40 Frame-Terminal-Left))))))

;; -------------------- SQL Frame ---------------------
(add-to-list 'display-buffer-alist
		   `("\\*SQL.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 30 Frame-Terminal-Top))
			  (left . ,(+ 30 Frame-Terminal-Left))))))

;; -------------------- Skewer Frame(s) ---------------------
;; TO DO: resize / auto-lower CLIENT & Error buffers
(add-to-list 'display-buffer-alist
		   `("\\*skewer-.*\\*"
			(display-buffer-reuse-window display-buffer-pop-up-frame)
			(reusable-frames . 0)
			(pop-up-frame-parameters
			 .
			 ((unsplittable . t)
			  (horizontal-scroll-bars . nil)
			  (vertical-scroll-bars . nil)
			  (height . ,Frame-Default-Height)
			  (width . ,Frame-Default-Width)
			  (top . ,(+ 20 Frame-Terminal-Top))
			  (left . ,(+ 20 Frame-Terminal-Left))))))

;; -------------------- YAS (New Snippet) ---------------------
;; (add-to-list
;;  'special-display-regexps
;;  (list ".*\\*new snippet.*\\*.*" 'display-*Help*-frame
;;        (list '(unsplittable . nil)
;;              '(horizontal-scroll-bars . nil)
;;              '(vertical-scroll-bars . nil)
;;              `(height . ,Frame-Default-Height)
;;              `(width . ,Frame-Default-Width)
;; 	     `(top . ,(+ 30 Frame-Terminal-Top))
;; 	     `(left . ,(- 30 Frame-Terminal-Left)))))


;; -------------------- Package Frame ---------------------
;; (add-to-list
;;  'special-display-regexps
;;  (list ".*\\*Packages.*\\*.*" 'display-default-frame
;;        (list '(unsplittable . nil)
;;              '(horizontal-scroll-bars . nil)
;;              '(vertical-scroll-bars . nil)
;;              `(height . ,Frame-Default-Height)
;;              `(width . ,(+ 100 Frame-Default-Width))
;; 	     `(top . ,Frame-Terminal-Top)
;; 	     `(left . ,(- 100 Frame-Terminal-Left)))))



(provide 'frame-settings)

;;; FRAME-SETTINGS.EL ends here

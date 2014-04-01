;; -----------------------------------------------------------------------------
;; FRAME-SETTINGS.EL --- Simple description
;; -----------------------------------------------------------------------------
;; Filename:		FRAME-SETTINGS.EL
;; Description:		Simple description
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Copyright (C)	2013, John P. Hilbert, all rights reserved.
;; Created:		2013-07-23 08:42:02
;; Version:		0.1
;; Last-Updated:	2013-07-23 08:42:02
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 23.3.1
;;
;; Features that might be required by this library:
;; <NONE>

;; !!!This file is NOT part of GNU Emacs!!!

;; -----------------------------------------------------------------------------
;; License
;; -----------------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; Simple description

;; 
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
(setq-default fit-frame-max-width Multi-Window-Default-Window-Width)

(defvar Multi-Window-Terminal-Window-Top
  5
  "Default terminal window top position")
(defvar Multi-Window-Terminal-Window-Left
  (/ (x-display-pixel-width) 2)
  "Default terminal window left position")

;; All of the above can customize by:
;;      M-x customize-group RET frame-settings RET
;;

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2013-07-23
;;      * First released.
;; 

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
;; <NONE>

;; -----------------------------------------------------------------------------
;; Code:
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
;;
;; See Also:
;;	http://www.gnu.org/s/emacs/manual/html_node/elisp/
;;	Window-Frame-Parameters.html  

;; -------------------- Default Function ------------------
(add-to-list 'default-frame-alist
	     `(height . ,Multi-Window-Default-Window-Height))
(add-to-list 'default-frame-alist
	     `(width . ,Multi-Window-Default-Window-Width))
(add-to-list 'default-frame-alist
	     `(top . ,Multi-Window-Default-Window-Top))
(add-to-list 'default-frame-alist
	     `(left . ,Multi-Window-Default-Window-Left))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; -------------------- Help Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*.*help.*\\*.*" 'display-*Help*-frame
       (list '(horizontal-scroll-bars . nil)
	     '(vertical-scroll-bars . nil)
	     '(height . 40)
	     '(top . 10)
	     (cons 'left (/ (x-display-pixel-width) 2)))))

;; -------------------- Occur Frame --------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*Occur.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
	     '(horizontal-scroll-bars . nil)
	     '(vertical-scroll-bars . nil)
	     '(height . 35)
	     `(width . ,Multi-Window-Default-Window-Width)
	     '(top . 10)
	     (cons 'left (+ (/ (x-display-pixel-width) 2) 20)))))

;; --------------- Shell / Power Shell Frame -----------
(add-to-list
 'special-display-regexps
 (list ".*\\*shell.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
             `(top . ,(+ 0 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 0 Multi-Window-Terminal-Window-Left)))))

(add-to-list
 'special-display-regexps
 (list ".*\\*PowerShell.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 10 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 10 Multi-Window-Terminal-Window-Left)))))

;; -------------------- R Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*R.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 20 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 20 Multi-Window-Terminal-Window-Left)))))

;; -------------------- Message Frame ---------------------
(add-to-list
 'special-display-buffer-names
 (list "*Messages*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 50 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 50 Multi-Window-Terminal-Window-Left)))))


;; -------------------- Backtrace Frame --------------------
(add-to-list
 'special-display-buffer-names
 (list "*Backtrace*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 30)
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 60 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 60 Multi-Window-Terminal-Window-Left)))))


;; -------------------- Python Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*Python.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)             
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 40 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 40 Multi-Window-Terminal-Window-Left)))))


;; -------------------- SQL Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*SQL.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
	     `(top . ,(+ 30 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 30 Multi-Window-Terminal-Window-Left)))))




(provide 'frame-settings)

;;; FRAME-SETTINGS.EL ends here

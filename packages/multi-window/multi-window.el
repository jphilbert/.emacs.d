;; -----------------------------------------------------------------------------
;; MULTI-WINDOW.EL --- Simple form of One-on-One
;; -----------------------------------------------------------------------------
;; Filename:		MULTI-WINDOW.EL
;; Description:		Simple form of One-on-One
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Copyright (C)	2012, John P. Hilbert, all rights reserved.
;; Created:		2012-02-13 20:30:16
;; Version:		0.2
;; Last-Updated:	2012-03-26
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 23.2.1
;;
;; Features that might be required by this library:
;;	http://www.emacswiki.org/emacs/frame-cmds.el
;;	http://www.emacswiki.org/emacs/frame-fns.el
;;	http://www.emacswiki.org/emacs/fit-frame.el

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
;; Simple form of One-on-One

;; This package is inspired from Drew Adam's OneOnOne Package and uses much of
;; his supplementary packages.  The key motivation was to streamline the
;; package and make it as transparent as possible.
;; 
;;      - Use of Special Display lists for frame configuration
;;              - Displaying frames require display-buffer
;;              - Some functions need redefined (ess-help.el has major issues)
;;      - Frames may or may not have minibuffer (i.e. *Help* doesn't need a one)
;;      - External programs (Windows Explorer) will open files in a new frame
;;
;; Added Features / Functions:
;;      - Fix an out-of-bounds frame (this may be unimportant if frames are
;;              created correctly in the first place)
;;      - Calculate frame coordinates easily
;;      - Last-Frame function for toggling between the recently used frame
;;      - Close a frame AND kill a buffer (replacement for C-x k)
;;

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put MULTI-WINDOW.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'multi-window)
;;	

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------
;; <NONE>
;;
;; All of the above can customize by:
;;      M-x customize-group RET multi-window RET
;;

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2012-02-13
;;      * First released.
;; 2012-03-14
;;	* added function to 'kill-buffer-or-emacs' for killing emacs if last
;;frame.
;; 2012-03-26
;;	* started adding rewrites for help functions.  For instance *Help*
;;displays correctly at first, however reuses the buffer hence does not refit on
;;subsequent displays.  Added a short (reusable piece to kill the frame if
;;active) 
;; 

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; Thanks to Drew Adams for his inspirational One-On-One Emacs package and
;;  helpers (http://www.emacswiki.org/emacs/OneOnOneEmacs)

;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
(require 'fit-frame)

;; -----------------------------------------------------------------------------
;; Variables
;; -----------------------------------------------------------------------------
(defcustom Multi-Window-Default-Window-Height 50
  "The default window (frame) height in characters")

(defcustom Multi-Window-Default-Window-Width 81
  "The default window (frame) width in characters")

(defcustom Multi-Window-Default-Window-Top 5
  "The default window (frame) top in characters")

(defcustom Multi-Window-Default-Window-Left 5
  "The default window (frame) left in characters")


;; -----------------------------------------------------------------------------
;; Setup
;; -----------------------------------------------------------------------------
;; Sets title bar as "<buffer>"
;;      Note: it sets it to this if there are multiple frames anyways
(setq frame-title-format "%b")

;; Change file opening to popup in other frame
(defalias 'find-file 'find-file-other-frame)
(setq-default display-buffer-reuse-frames t) ; ??? Don't remember why ???

;; When opening a file from outside emacs the server processes the request, this
;;  handles creating a new frame for the buffer
(add-hook 'server-visit-hook 'new-frame)

;; Delete all frames showing buffer after kill-buffer
(add-hook 'kill-buffer-hook 'delete-windows-on)

;; Make the GUI close (X) button act like kill-buffer
(defadvice handle-delete-frame (around delete-frame-after-kill activate)
  "Map (X) button to kill-buffer"
  (let ((frame   (posn-window (event-start event))))
    (kill-buffer-or-emacs (window-buffer (get-largest-window frame)))))

;; This is why I have problems with *shell*.  Since I do not use any of the
;;  other buffers just set to nil
(setq same-window-buffer-names nil)
(setq-default fit-frame-max-height-percent 70)
  

;; -----------------------------------------------------------------------------
;; Display functions
;; -----------------------------------------------------------------------------
(defun display-default-frame (buf &optional args) 
  "Default (vanilla) display function.  Displays buffer in its
own frame and double checks if it is within bounds."
  (let ((return-window (select-window
                        (funcall special-display-function buf args)))
        (frame (raise-frame)))
    
    (set-window-buffer (frame-selected-window frame) buf)
    (set-window-dedicated-p (frame-selected-window frame) nil)
    (frame-selected-window frame)
    (fix-frame-bounds frame)
    
    return-window))

(defun display-default-frame (buf &optional args)
  (if (and args (symbolp (car args)))
      (apply (car args) buffer (cdr args))
    
    (let ((window (get-buffer-window buffer 0)))
      (or
       ;; If we have a window already, make it visible.
       (when window
         (let ((frame (window-frame window)))
           (make-frame-visible frame)
           (raise-frame frame)
           window))
       
       ;; Reuse the current window if the user requested it.
       (when (cdr (assq 'same-window args))
         (condition-case nil
             (progn (switch-to-buffer buffer) (selected-window))
           (error nil)))
       
       ;; Stay on the same frame if requested.
       (when (or (cdr (assq 'same-frame args)) (cdr (assq 'same-window args)))
         (let* ((pop-up-windows t)
                pop-up-frames
                special-display-buffer-names special-display-regexps)
           (display-buffer buffer)))
       
       ;; If no window yet, make one in a new frame.
       (let ((frame
              (with-current-buffer buffer
                (make-frame (append args special-display-frame-alist)))))
         (set-window-buffer (frame-selected-window frame) buffer)
         (set-window-dedicated-p (frame-selected-window frame) t)
         (frame-selected-window frame))))))

(defun display-*Help*-frame (buf &optional args)
  "Display *Help* buffer in its own frame.
`special-display-function' is used to do the actual displaying.
BUF and ARGS are the arguments to `special-display-function'."
  (let ((calling-frame (selected-frame))
        (return-window (select-window
                        (funcall special-display-function buf args))))
    (raise-frame)
    (fit-frame)                         ; Fit the frame to its context
    (setq mode-line-format nil)         ; Remove Mode Line
    (raise-frame calling-frame)
    return-window))

;; ----------------------------------------------------------------------------
;; Function Definitions
;; ----------------------------------------------------------------------------
(defun frame-coordinates (&optional frame)
  "Yields the frame coordinates in pixels as a list.  The order
is LEFT TOP RIGHT BOTTOM"
  (let ((left (frame-parameter frame 'left))
        (top (frame-parameter frame 'top))
        (width (frame-pixel-width frame))
        (height (frame-pixel-height frame))
        right
        bottom)
    
    (when (consp left)
      (setq left (car (last left))))
    
    (when (consp top)
      (setq top (car (last top))))
    
    (setq right (+ left width))
    (setq bottom (+ top height))
    
    (list left top right bottom)))

(defun fix-frame-bounds (&optional frame lock-corner)
  "Fits the frame within the screen boundary.  It will attempt to
move the frame prior to changing the size unless `lock-corner' is
true."
  (let ((this-frame-coords (frame-coordinates frame))
        left
        top
        right
        bottom
        (width (frame-pixel-width frame))
        (height (frame-pixel-height frame))
        (max-x (- (x-display-pixel-width) 8)) ; Notice Offset
        (max-y (- (x-display-pixel-height) 30))
        (char-width (frame-char-width frame))
        (char-height (frame-char-height frame)))
    
    (when (not frame)
      (setq frame (previous-frame)))
    
    (setq left (pop this-frame-coords)
          top (pop this-frame-coords)
          right (pop this-frame-coords)
          bottom (pop this-frame-coords))
    
    ;; Check if dim is out of bounds
    (when (or (< left   0)
              (< top    0)
              (> right  max-x)
              (> bottom max-y))
      
      ;; Can it be fixed by moving
      ;; & Should we fix it by moving
      ;;        Shift Left
      (when (and (<= width max-x)
                 (> right max-x)
                 (not lock-corner))
        (setq right max-x
              left (- right width)))
      
      ;;        Shift Up
      (when (and (<= height max-y)
                 (> bottom max-y)
                 (not lock-corner))
        (setq bottom max-y
              top (- bottom height)))
      
      ;;        Shift Right
      (when (< left 0)
        (setq left 0
              right width))
      
      ;;        Shift Down
      (when (< top 0)
        (setq top 0
              bottom height))
      
      (set-frame-position frame left top)
      
      ;; Does it need Sizing adjustments
      (when (> right max-x)             ; Note offset
        (set-frame-width frame (- (/ (- max-x left) char-width) 2)))
      
      (when (> bottom max-y)
        (set-frame-height frame (/ (- max-y top) char-height))))))

(defun get-frame (&optional name-buffer-window)
  "Gets the frame from a string, buffer, or window."
  (unless (framep name-buffer-window)
    (unless (windowp name-buffer-window)
      (setq name-buffer-window (get-buffer-window name-buffer-window t)))
    (setq name-buffer-window (window-frame name-buffer-window))))

(defun kill-buffer-or-emacs (&optional args)
    "Kills a buffer or emacs if it is the last frame. Note: no saving is done."
    (interactive)
    (if (eq (length (frame-list)) 1)
	(kill-emacs)
      (kill-buffer args)))



(provide 'multi-window)
;; ;;; MULTI-WINDOW.EL ends here



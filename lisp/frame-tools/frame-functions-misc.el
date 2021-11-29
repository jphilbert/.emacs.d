;; -----------------------------------------------------------------------------
;; FRAME-FUNCTIONS-MISC.EL --- Miscellaneous Frame Functions
;; -----------------------------------------------------------------------------
;; Filename:		FRAME-FUNCTIONS-MISC.EL
;; Description:	Miscellaneous Frame Functions
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2012-02-13 20:30:16
;; Last-Updated:	2014-04-05
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 23 - 24.3
;;

;; ---------- !!!This file is NOT part of GNU Emacs!!! ----------

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; Various functions that may be useful for managing and altering frames.
;;

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put FRAME-FUNCTIONS-MISC.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'frame-functions-misc)
;;	

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2014-04-05
;;	* Previously multi-window.el
;;	* Migrated various misplaced functions here
;; 

;; ----------------------------------------------------------------------------
;; Function Definitions
;; ----------------------------------------------------------------------------
(defalias 'kill-frame 'delete-frame)

(defun kill-duplicate-frames ()
  (interactive)
  (let* ((frame-list (frame-list))
	    (current-frame (selected-frame))
	    (result
		(delq nil
			 (mapcar
			  (lambda (frame)
			    (with-selected-frame frame
				 (when (one-window-p)
				   (with-current-buffer (car (buffer-list frame))
					;; (message (buffer-name))
					(list frame (buffer-name))))))
			  frame-list)))
	    (destructable-result (copy-list result)))
    
    (mapc
    	(lambda (x)
    	  (setq destructable-result (delq x destructable-result))
    	  (mapc
    	   (lambda (y)
    		(when (equal (cdr x) (cdr y))
    		  (if (eq (car x) current-frame)
                (when (frame-live-p (car y))
                  (delete-frame (car y))
			   ;; (message "%s" y)
                  (setq destructable-result (delq y destructable-result)))
    		    (when (frame-live-p (car x))
    			 (delete-frame (car x))
			 ;; (message "%s" x)
			 ))))
    	   destructable-result))
    	result)
    )
  )

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

(defun end-of-buffer-all ()
  "Moves the cursor to the end for all windows showing current buffer."
  (interactive)
  (goto-char (point-max))
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows)))))

(defun save-frame-excursion (&rest x)
  "Like save-window-excursion, however restores current frame"
  (let (this-frame (selected-frame))
    (save-window-excursion x)
    (raise-frame this-frame)))

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

(defun frame-cascade-vertical-value ()
  (let*
	 ((current-pos
	   (frame-parameter nil 'top))
	  (display-pos
	   (nth 1 (cdr (assoc 'workarea (frame-monitor-attributes)))))
	  (workarea-size
	   (nth 3 (cdr (assoc 'workarea (frame-monitor-attributes)))))
	  (frame-size
	   (* (frame-char-height) (frame-parameter nil 'height)))
	  (max-offset (- workarea-size frame-size
				  32				; Task Bar Height
				  )))
    
    ;; Typically the position is a list of form (+ . INT)
    (when (consp current-pos)
      (setq current-pos (car (last current-pos))))

    ;; Make the position relative to this display
    (setq current-pos (- current-pos display-pos))
    
    (setq current-pos
		(cond
		 ;; If we have room, offset
		 ((> max-offset (+ current-pos Frame-Cascade-Offset))
		  (+ current-pos Frame-Cascade-Offset))
		 ;; Else offsetting will cut some of the frame so reset the window
		 ;; back to zero and offset the surplus
		 ((> (* 2 max-offset) (+ current-pos Frame-Cascade-Offset))
		  (- (+ current-pos Frame-Cascade-Offset) max-offset))
		 ;; There is a chance that this surplus will still yield some cutting
		 ;; of the frame (typical on small displays) so either put the frame
		 ;; to the minimum or maximum (which ever its farthest)
		 ((> (/ max-offset 2) current-pos)
		  max-offset)
		 (t 0)))

    `(+ ,(+ current-pos display-pos))
    ))

(defun frame-cascade-horizontal-value ()
  (let*
	 ((current-pos
	   (frame-parameter nil 'left))
  	  (display-pos
	   (nth 0 (cdr (assoc 'workarea (frame-monitor-attributes)))))
	  (workarea-size
	   (nth 2 (cdr (assoc 'workarea (frame-monitor-attributes)))))
	  (frame-size
	   (* (frame-char-width) (frame-parameter nil 'width)))
	  (max-offset (- workarea-size frame-size)))

    ;; Typically the position is a list of form (+ . INT)
    (when (consp current-pos)
      (setq current-pos (car (last current-pos))))

    ;; Make the position relative to this display
    (setq current-pos (- current-pos display-pos))

    (setq current-pos
		(cond
		 ;; If we have room, offset
		 ((> max-offset (+ current-pos Frame-Cascade-Offset))
		  (+ current-pos Frame-Cascade-Offset))
		 ;; Else offsetting will cut some of the frame so reset the window
		 ;; back to zero and offset the surplus
		 ((> (* 2 max-offset) (+ current-pos Frame-Cascade-Offset))
		  (- (+ current-pos Frame-Cascade-Offset) max-offset))
		 ;; There is a chance that this surplus will still yield some cutting
		 ;; of the frame (typical on small displays) so either put the frame
		 ;; to the minimum or maximum (which ever its farthest)
		 ((> (/ max-offset 2) current-pos)
		  max-offset)
		 (t 0)))

    `(+ ,(+ current-pos display-pos))
    ))

(defun frame-cascade (&optional frame)
  (interactive)
  (unless
	 ;; Inhibit special buffers
	 (display-buffer-assq-regexp
	  (buffer-name (car (frame-parameter frame 'buffer-list)))
	  display-buffer-alist nil)
    (set-frame-parameter frame 'left (frame-cascade-horizontal-value))
    (set-frame-parameter frame 'top (frame-cascade-vertical-value))))

(provide 'frame-functions-misc)
;; ;;; FRAME-FUNCTIONS-MISC.EL ends here



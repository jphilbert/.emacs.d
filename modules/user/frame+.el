;; -----------------------------------------------------------------------------
;; FRAME+.EL --- Miscellaneous Frame Functions
;; -----------------------------------------------------------------------------
;; Filename:		FRAME+.EL
;; Description:	Miscellaneous Frame Functions
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Created:		2012-02-13 20:30:16
;; Last-Updated:    2022-08-01
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
;; ---------- Conventions ---------- ;;
;;	- all functions, variables, commands begin with FRAME
;;	- frame arguments will be first in the argument list
;;   - frame arguments will be optional, defaulting to the current
;;	- coordinates will be in pixels relative to the main display
;;	- sizes will be in pixels 

(require 'dash+)

;; ------------------------------------------------------------------------- ;;
;; Frame <--> Window / Buffer Methods
;; ------------------------------------------------------------------------- ;;
(defun frame-get (&optional name-buffer-window)
  "Gets the frame from a string, buffer, or window.

This handles various special cases:
	- If STRING is not a name of a buffer --> nil
	- If BUFFER does not have a window (hence a frame) --> nil
	- If FRAME --> FRAME
	- If NIL (no argument) --> current frame
	- If any other OBJECT --> nil
"  
  (cond ((not name-buffer-window)
	    (selected-frame))
	   ((framep name-buffer-window)
	    name-buffer-window)
	   ((windowp name-buffer-window)
	    (window-frame name-buffer-window))
	   ((or (stringp name-buffer-window)
		   (bufferp name-buffer-window))
	    (setq name-buffer-window
			(get-buffer-window name-buffer-window t))
	    (when name-buffer-window
		 (window-frame name-buffer-window)))))

(defalias 'frame-get-window
  'frame-root-window
  "Return the main (root) window in FRAME")

(defun frame-get-buffer (&optional frame)
  "Return the buffer in the main window in FRAME"
  (window-buffer (frame-get-window frame)))


;; ------------------------------------------------------------------------- ;;
;; Frame Attributes 
;; ------------------------------------------------------------------------- ;;
(defun frame-size (&optional frame x y)
  "Gets or sets the width (X) and height (Y) of FRAME in pixels.

This function combines `set-frame-width' and `set-frame-height' in a user
friendly manner such that all arguments are optional and in any order. The first
2 whole numbers / nulls will be used as X and Y unless given as a list. The
selected frame is used if a FRAME is not given.

Returns a list of the width and height of FRAME"
  (-let*
	 ((arguments	(list frame x y))
	  (frame		(or (-first #'framep arguments)
				    (selected-frame)))
	  (x			(--first (and (listp it) it) arguments))
	  ((x y)		(or x
				    (--filter (or (natnump it) (null it)) arguments))))
    
    (when x 
	 (set-frame-width frame x nil t))
    (when y
	 (set-frame-height frame y nil t))

    (list (frame-inner-width	frame)
		(frame-inner-height	frame))))

(defun frame-position (&optional frame x y)
  "Gets or sets the upper left (X, Y) coordinates of FRAME in pixels.

This function uses `set-frame-parameter' in a user friendly manner such that all
arguments are optional and can be in any order. The first 2 whole numbers /
nulls will be used as X and Y unless given as a list. Null values denote to
leave untouched. The X position is shifted left by the external border size such
the FRAME position would be equivalent to one that is maximized. The selected
frame is used if a FRAME is not given.

Returns a list of upper left (X, Y) coordinates of FRAME"
  (-let*
	 ((arguments	(list frame x y))
	  (frame		(or (-first #'framep arguments)
				    (selected-frame)))
	  (x			(--first (and (listp it) it) arguments))
	  ((x y)		(or x
				    (--filter (or (integerp it) (null it)) arguments)))
	  
	  ((&alist
	    'outer-position			(current-x .	current-y)
	    'external-border-size	(border-x .	border-x))
	   (frame-geometry frame))

	  (current-x	(or x (+ current-x border-x)))
	  (current-y	(or y current-y)))

    (when x
	 (set-frame-parameter frame 'left `(+ ,(- current-x border-x))))
    (when y
	 (set-frame-parameter frame 'top  `(+ ,current-y)))

    (list current-x current-y)))

(defun frame-character-size (&optional frame)
  "Get the width and height as a list of a character in FRAME"
  (list (frame-char-width	frame)
	   (frame-char-height	frame)))

(defun frame--window-padding (&optional frame)
  "Gets the difference between the FRAME and the main window sizes.

The padding is specifically the differences:
	`frame-inner-width' - `window-body-width' and
	`frame-inner-height' - `window-body-height'
Additionally if either the left or right fringe is 0, the character width is
added to the X-padding.

This is primarily used in `frame-fit-to-buffer'."
  (-let* (((size-x size-y)	(frame-size frame))
		(window			(frame-get-window frame))
		;; If there is no fringes add a character for line-continuation
		((fringe-l fringe-r) (window-fringes window))
		(no-fringe (if (and (> fringe-l 0) (> fringe-r 0))
					0
				   (- (frame-char-width frame)))))
    (list (- size-x (window-body-width	window t) no-fringe)
		(- size-y (window-body-height window t)))))

(defun frame--min-size (&optional frame)
  "Gets the minimum size of the FRAME.

This is equal to the minimum size of the main window of the FRAME and adding any
additional padding between the window and frame size."
  (-let* (((size-x size-y)	(frame-size frame))
		(window			(frame-get-window frame)))
    (list (+ (- size-x (window-pixel-width window))
		   (window-min-size (frame-get-window frame) t nil t))
		(+ (- size-y (window-pixel-height window))
		   (window-min-size (frame-get-window frame) nil nil t)))))


;; ------------------------------------------------------------------------- ;;
;; Display Attributes 
;; ------------------------------------------------------------------------- ;;
(defun frame-display-position (&optional frame)
  "Gets the upper left coordinates of display FRAME is on."
  (-take 2 (frame-monitor-workarea frame)))

(defun frame-display-size (&optional frame)
  "Gets the usable size of display FRAME is on.

This the usable size of the frame is calculated by:
	X = work area width
	Y = work area height - border height - title height"
  (-let* (((&alist
		  'external-border-size	(border-x . border-y)
		  'title-bar-size		(_ . title))
		 (frame-geometry frame)))
    (--update-at 1
			  (- it border-y title 1)
			  (-take-last 2 (frame-monitor-workarea frame)))))

(defun frame-display-left-position ()
  "Returns the coordinates of the left most (minimum X) display"
  (->> (display-monitor-attributes-list)
	  (--map (-slice (alist-get 'workarea it) 0 2))
	  (--min-by (> (car it) (car other)))))

(defun frame-display-top-position ()
  "Returns the coordinates of the top most (minimum Y) display"
  (->> (display-monitor-attributes-list)
	  (--map (-slice (alist-get 'workarea it) 0 2))
	  (--min-by (> (nth 1 it) (nth 1 other)))))


;; ------------------------------------------------------------------------- ;;
;; Window Functions
;; ------------------------------------------------------------------------- ;;
(defun frame-window-count (&optional frame)
  "Return count of windows in FRAME"
  (length (window-list frame)))

(defun frame-one-window (&optional frame)
  "Return non-nil if FRAME has only one window"
  (= (frame-window-count frame) 1))

(defun frame-unsplit (&optional frame)
  "Deletes all windows except for the largest in FRAME"
  (interactive)
  (delete-other-windows (frame-get-window frame)))


;; ------------------------------------------------------------------------- ;;
;; 
;; ------------------------------------------------------------------------- ;;
(defun frame-list-with-buffer (&optional buffer)
  "Returns list of all frames showing BUFFER (current by default)"
  (->> (get-buffer-window-list buffer nil t)
	  (-map #'window-frame)
	  (-distinct)))

(defalias 'frame-new 'make-frame
  "Creates a new frame")


;; ------------------------------------------------------------------------- ;;
;; Killing Frames and Buffers
;; ------------------------------------------------------------------------- ;;
(defalias 'frame-kill 'delete-frame
  "Deletes FRAME")

(defun frame-kill-duplicates (&optional buffer)
  "Deletes any frames showing BUFFER except the current"
  (interactive)
  (--each 
   (-remove-item (selected-frame) (frame-list-with-buffer buffer))
   (frame-kill it t)))

(defun frame-kill-buffer (&optional buffer)
    "Kills a buffer or emacs if it is the last frame. Note: no saving is done."
    (interactive)
    (if (eq (length (frame-list)) 1)
	(kill-emacs)
      (kill-buffer buffer)))

(defun frame-kill-dwim ()
  (interactive)
  (cond
   ;; 1 frame + 1 window --> save buffer + kill emacs
   ((= (length (frame-list)) 1)
    (kill-emacs))
   
   ;; N frame
   ;;		+ 1 window 
   ((one-window-p)
    ;;				+ 1 buffer-window --> kill buffer (kill frame)
    (if (= (length (get-buffer-window-list nil nil t)) 1)
	   (kill-buffer)
	 ;;				+ N buffer-window --> kill frame
	 (frame-kill)))
   
   ;;		 + N windows
   ;;				 + active buffer in N windows --> kill window
   ((> (length (get-buffer-window-list nil nil t)) 1)
    (delete-window))
   ;; ;;				 + active buffer not in upper left --> kill window
   ;; ((equal (frame-first-window) (selected-window))
   ;;  (delete-window))
   
   ;; OTHERWISE --> kill other windows
   (t
    (delete-other-windows))
   )
  )


;; ------------------------------------------------------------------------- ;;
;; Banish Buffers
;;	Kind of like iconifying but without inconifying. Changing the visibility a
;;	frame also raises it which at times we would prefer not to occur. Banishing
;;	simply moves the frame to an extreme position off screen hence only
;;	alteration is its position.
;; ------------------------------------------------------------------------- ;;
(defun frame-banish (&optional frame)
  "If on screen, moves FRAME to an extreme position (-20k, -20k) off screen"
  (unless (frame-parameter frame 'off-screen)
    (set-frame-parameter frame 'banished-position (frame-position frame))
    (frame-position frame (-repeat 2 -20000))))

(defun frame-unbanish (&optional frame)
  "If off screen, moves FRAME to its previous position or (0, 0)"
  (when (frame-parameter frame 'off-screen)
    (frame-position frame
				    (or (frame-parameter frame 'banished-position)
					   (-repeat 2 0)))
    (set-frame-parameter frame 'banished-position nil)))

(defun frame-on-screen (&optional frame)
  "Identifies if some portion of FRAME is shown on its display.

This function has the added effect of setting the frame parameter
`off-screen' if no portion of the FRAME is on screen"
  (-let*
	 (;; Upper Left (minimum) corner of display
	  ((pos &as dX0 dY0)
	   (frame-display-position frame))
	  ;; Lower Right (maximum) corner of display
	  ((dX1 dY1)
	   (-mapcar #'+ pos (frame-display-size frame)))

	  ;; Upper Left (minimum) corner of frame
	  ((pos &as fX0 fY0)
	   (frame-position frame))
	  ;; Lower Right (maximum) corner of frame
	  ((fX1 fY1)
	   (-mapcar #'+ pos (frame-size frame)))

	  (on-screen 
	   (or
	    ;; Left			Right
	    (<= dX0 fX0 dX1)	(<= dX0 fX1 dX1)
	    ;; Top			Bottom
	    (<= dY0 fY0 dY1)	(<= dY0 fY1 dY1))))

    (set-frame-parameter (or frame (selected-frame))
					'off-screen (not on-screen))
    on-screen))

(defun frame-on-screen-completely (&optional frame)
  "Identifies if all of the FRAME is shown on its display."
  (-let*
	 (;; Upper Left (minimum) corner of display
	  ((pos &as dX0 dY0)
	   (frame-display-position frame))
	  ;; Lower Right (maximum) corner of display
	  ((dX1 dY1)
	   (-mapcar #'+ pos (frame-display-size frame)))

	  ;; Upper Left (minimum) corner of frame
	  ((pos &as fX0 fY0)
	   (frame-position frame))
	  ;; Lower Right (maximum) corner of frame
	  ((fX1 fY1)
	   (-mapcar #'+ pos (frame-size frame))))
    
    (and
	;; Left			Right
	(<= dX0 fX0 dX1)	(<= dX0 fX1 dX1)
	;; Top			Bottom
	(<= dY0 fY0 dY1)	(<= dY0 fY1 dY1))))

;; Sets `on-screen' parameter automatically when a frame moves
(add-hook 'move-frame-functions #'frame-on-screen)


;; ------------------------------------------------------------------------- ;;
;; Ordering Frames by Use
;; ------------------------------------------------------------------------- ;;
(defvar frame-list-select-order (list)
  "List of frames in order of use")

(defun frame-get-order ()
  "Gets `frame-list-select-order' with the selected frame at the top.

This does not alter `frame-list-select-order' list (see `frame-update-order' for
updating the order)."
	  (->> frame-list-select-order
		  (-filter #'frame-live-p)
		  (-remove-item (selected-frame))
		  (-cons* (selected-frame))))

(defun frame-update-order ()
  "Adds the current frame to `frame-list-select-order'.

This calls `frame-get-order' and sets `frame-list-select-order' its return
value. This function should simply be added to `after-focus-change-function'
like:

(add-function :after after-focus-change-function #'frame-update-order)"
  (setq frame-list-select-order (frame-get-order)))

(add-function :after after-focus-change-function #'frame-update-order)

(defun frame-get-last (&optional frame-predicate)
  "Returns the last selected frame satisfying FRAME-PREDICATE."
  (let ((predicate (or frame-predicate #'frame-live-p)))
    (-first predicate frame-list-select-order)))

(defun frame-display-last ()
  "Displays the last selected frame."
  (interactive)
  (raise-frame (frame-get-last)))


;; ------------------------------------------------------------------------- ;;
;; Fitting Frames
;; ------------------------------------------------------------------------- ;;
(defun frame--calculate-size-range (&optional frame extents margins)
  "Calculate the maximum/minimum size of FRAME and margin to reserve.

Preferred size are taken either from the EXTENTS (Y-max, Y-min, X-max, X-min)
argument, `fit-frame-to-buffer-sizes' frame-parameter, or the
`fit-frame-to-buffer-sizes' global value. These are given in character
dimensions.

Similarly, margins are taken either from MARGINS (left, top, right, bottom)
argument or from `fit-frame-to-buffer-margins'. These are expected to be given
in pixels.

The function returns a list of (EXTENTS, MARGINS) in pixels with the proper
ordering."
  ;; Note that EXTENTS are backwards than expected (MAX-Y MIN-Y MAX-X MIN-X)
  ;; MARGINS are as usual (LEFT, TOP, RIGHT, BOTTOM)
  (-let*
	 (
	  ;; Display Size
	  (display-size	(frame-display-size frame))
	  ;; Minimum Frame Size
	  (min-size		(frame--min-size frame))
	  
	  (surplus-size	(-mapcar #'- display-size min-size))

	  ;; Margins to keep around frame (left, top, right, bottom)
	  ((margin-x0
	    margin-y0
	    margin-x1
	    margin-y1)		(->> (or margins
						    (frame-parameter
							frame 'fit-frame-to-buffer-margins)
						    fit-frame-to-buffer-margins)
	    (-pad-n 0 4)
	    (--map-when (not it) 0)))
	  
	  ;; Make sure top left is less than surplus
	  (margin			(->> (list margin-x0 margin-y0)
						(-mapcar #'min surplus-size)))
	  ;; Calculate (x0+x1, y0+y1) and make sure less than surplus
	  (margin-size		(->> (list margin-x1 margin-y1)
						(-mapcar #'+ margin)
						(-mapcar #'min surplus-size)))
	  ;; Recalculate margin around frame for later use
	  (margin			(->> margin
						(-mapcar #'- margin-size)
						(-concat margin)))
	  ;; Calculate max size 
	  ;; -> display size - margin size 
	  ;;	-> flip such it is Y, X
	  ;; -> interleave nil as minimum
	  (margin-size		(-> (-mapcar #'- display-size margin-size)
					    (reverse)
					    (-interleave '(nil nil))))

	  ;; Frame Parameters (in character sizes)
	  (frame-size		(->> (or extents
						    (frame-parameter
							frame 'fit-frame-to-buffer-sizes)
						    fit-frame-to-buffer-sizes)
						(-pad-n 0 4)
						(--map-when (not it) 0)))
	  
	  ;; Convert to pixels
	  (frame-size		(->>
					 (frame-character-size frame)
					 (reverse)
					 (--map (-repeat 2 it))
					 (-flatten)
					 (-mapcar #'* frame-size)
					 (--map-when (< it 1) nil)))

	  ;; Absolute max-min
	  (defaults		(-interleave (reverse display-size)
							   (reverse min-size)))
	  
	  (frame-size		(->>
					 (-zip-lists frame-size defaults)
					 (--map (-first #'numberp it))
					 (-zip-lists margin-size)
					 (--map (-min (-non-nil it))))))
    
    (list frame-size margin)))

(defun frame-fit-to-buffer (&optional frame extents margins only)
  "Adjust size of FRAME to display the contents of its buffer exactly.  FRAME
can be any live frame and defaults to the selected one.  Fit only if FRAME's
root window is live.

MAX-HEIGHT, MIN-HEIGHT, MAX-WIDTH and MIN-WIDTH specify bounds on the new total
size of FRAME's root window.  MIN-HEIGHT and MIN-WIDTH default to the values of
`window-min-height' and `window-min-width' respectively.  These arguments are
specified in the canonical character width and height of FRAME.

If the optional argument ONLY is `vertically', resize the frame vertically only.
If ONLY is `horizontally', resize the frame horizontally only.

The new position and size of FRAME can be additionally determined by customizing
the options `fit-frame-to-buffer-sizes' and `fit-frame-to-buffer-margins' or
setting the corresponding parameters of FRAME.

Any leading or trailing empty lines of the buffer content are not considered."
  (interactive)
  (unless (fboundp 'display-monitor-attributes-list)
    (user-error "Cannot resize frame in non-graphic Emacs"))
  
  (setq frame (frame-get frame))

  (-let*
	 (
       ;; WINDOW is FRAME's root window.
       (window			(frame-root-window frame))
       (display-size		(frame-display-size frame))
	  (display-position	(frame-display-position frame))		
	  (position			(frame-position frame))
       
	  ((extents margins)	(frame--calculate-size-range
						 frame extents margins))

	  (min-size			(-select-by-indices '(3 1) extents))
	  (max-size			(-select-by-indices '(2 0) extents))
	  (margins			(list
						 (-to-cons
						  (-select-by-indices '(0 2) margins))
						 (-to-cons
						  (-select-by-indices '(1 3) margins))))
	  
	  (window-pad		(frame--window-padding frame))
	  ((max-pad-size-x
	    max-pad-size-y)	(-mapcar #'- max-size window-pad))
	  
	  ;; Note: Currently, for a new frame the sizes of the header and mode
       ;; line may be estimated incorrectly
       (size			(->> (window-text-pixel-size
						 window nil nil
						 max-pad-size-x
						 max-pad-size-y)
						(flatten-tree)
						(-mapcar #'+ window-pad)
						(-mapcar #'max min-size)
						(-mapcar #'min max-size)))

	  ;; Round to Character Size (if needed)
	  (size			(if frame-resize-pixelwise
					    (-mapcar (lambda (s c)
								(* (/ (+ s -1 c) c) c))
							   size (frame-character-size frame))
					  size))
	  
	  ;; Don't change height or width when the window's size is fixed in
	  ;; either direction or ONLY forbids it.
	  (fixed-size		(buffer-local-value
					 'window-size-fixed
					 (frame-get-buffer frame)))		
	  (size		(cond
				 ((or (eq fixed-size 'width)
					 (eq only 'vertically))
				  (-replace-at 0 nil size))
				 ((or (eq fixed-size 'height)
					 (eq only 'horizontally))
				  (-replace-at 1 nil size))
				 (t size)))
	  

	  ;; Check and move FRAME to preserve margins
	  (position	(-mapcar
				 (lambda (p s m ds dp)
				   (cond
				    ;; UPPER/LEFT off screen --> shift DOWN/RIGHT
				    ;; POSITION < DISPLAY-POSITION + MARGIN_0
				    ((and s
						(< p (+ (car m) dp)))
					(+ (car m) dp))

				    ;; LOWER/RIGHT off screen --> shift UP/LEFT
				    ;; POSITION + FRAME-SIZE >
				    ;;	DISPLAY-POSITION + DISPLAY-SIZE - MARGIN_1
				    ((and s
						(> (+ p s) (+ ds dp (- (cdr m)))))
					(max (- (+ ds dp) s (cdr m))
						;; shift but no farther than upper left corner
						(+ (car m) dp)))

				    ;; Both corners are fine --> no change
				    (t p)))
				 position size margins
				 display-size display-position)))

    (frame-position frame position)
    (frame-size frame size)))



;; ------------------------------------------------------------------------- ;;
;; Redisplay Buffers
;; ------------------------------------------------------------------------- ;;
(defun frame-redisplay-all (predicate)
  "Redisplays all buffers that satisfy PREDICATE."
  (delete-other-frames)
  (frame-position (frame-display-left-position))
  
  (let ((this-buffer (frame-get-buffer)))
    (--each (-filter predicate (buffer-list))
      (raise-frame (frame-get (display-buffer it))))
    
    (frame-kill (frame-get this-buffer))
    (when (funcall predicate this-buffer)
      (display-buffer this-buffer))))

;; ---------- Commands ---------- ;;
(defun frame-redisplay-all-file-buffers ()
  "Cascades all file buffer frames from the left corner."
  (interactive)
  (frame-redisplay-all #'buffer-file-name))


;; ------------------------------------------------------------------------- ;;
;; Cascading of Frames
;; ------------------------------------------------------------------------- ;;
(defvar frame-near-by-threshold 1
  "Distance defining if frames are near each other. See method `frame-near-by'")

(defun frame-near-by (&optional frame-or-position threshold)
  "Identifies if FRAME-OR-POSITION is within THRESHOLD of another frame.

The predicate returns true if some other frame position is within a distance of
FRAME-OR-POSITION. Specifically if | Xi - X | <= T AND | Yi - Y | <= T for any
frame i within a given threshold T.

FRAME-OR-POSITION can given as a frame, list (x, y), or null (selected
frame). THRESHOLD if null will use `frame-near-by-threshold' by default."
  (let*
	 ((frame		(cond
				 ((framep frame-or-position)
				  frame-or-position)
				 (frame-or-position
				  nil)
				 (t
				  (selected-frame))))
	  
	  (position	(if frame
				    (frame-position frame)
				  frame-or-position))
	  
	  (threshold	(or threshold frame-near-by-threshold 1)))
    
    (->> (frame-list)
	    (-remove-item frame)
	    (-map #'frame-position)
	    (--map (-mapcar (lambda (x y) (abs (- x y))) position it))
	    (-map #'-max)
	    (--some (<= it threshold)))))


(defvar frame-cascade-offset 25
  "Pixels to offset frame for cascading")

(defun frame--cascade-calculate-value (&optional frame position offset)
  "Calculates the location of FRAME after cascading it from POSITION by OFFSET.

Cascade location will be OFFSET pixels right and down from POSITION (defaults to
FRAME's position). If the shift puts an edge of the FRAME (default to current
position) off screen, that coordinate will wrap around from the other display
edge."
  (let*
	 ((frame-pos		(or position (frame-position frame)))
	  (display-pos		(frame-display-position frame))	  
	  (extra-space		(-mapcar #'-
						    (frame-display-size frame)
						    (frame-size frame)))
	  (offset			(or offset frame-cascade-offset 25)))
    
    (-mapcar 
	(lambda (p dp s)
	  (cond
	   ;; If we have room, offset
	   ;;		max > relative + offset --> relative + offset
	   ((> s (- (+ p offset) dp))
	    (+ p offset))
	   
	   ;; Else offsetting will cut some of the frame so reset the window
	   ;; back to zero and offset the surplus
	   ;;		2 * max > relative + offset --> relative + offset - max
	   ((> (* 2 s) (- (+ p offset) dp))
	    (- (+ p offset) s))
	   
	   ;; There is a chance that this surplus will still yield some cutting
	   ;; of the frame (typical on small displays) so either put the frame
	   ;; to the minimum or maximum (which ever its farthest)
	   ;;		max / 2 > relative --> max 
	   ((> (/ s 2) (- p dp))
	    (+ s dp))
	   ;;		relative > max / 2 --> 0
	   (t dp)))
	  frame-pos
	  display-pos
	  extra-space)))

(defun frame-cascade (&optional frame position offset ignore-near-frames)
  "Moves FRAME from POSITION by OFFSET intelligently.

The new location will be OFFSET (defaults to `frame-cascade-offset') pixels
right and down from POSITION (defaults to FRAME's position). If the shift puts
an edge of the FRAME (default to current position) off screen, that coordinate
will wrap around from the other display edge.

By default, the method will cascade until a frame is not found near (see
`frame-near-by') the the new position. To disable this behavior set
`ignore-near-frames' to non-nil."
  (let*
	 ((position		(frame--cascade-calculate-value frame position offset))
	  (search-depth	100))
    
    (while (and (not ignore-near-frames)			 
			 (frame-near-by position)
			 (> search-depth 0))
	 (setq position	(frame--cascade-calculate-value frame position offset))
	 (setq search-depth	(1- search-depth)))
    
    (frame-position frame position)
    position))

(defun frame-cascade-frames (&optional predicate position offset)
  "Cascades all frames equaling PREDICATE by OFFSET from POSITION.

This helper function will move all frames satisfying FRAME-PREDICATE in reverse order such that the oldest frame will be at INITIAL-POSITION."
  (-let*
	 ((position	(or position
				    (frame-display-left-position)))
	  (predicate	(or predicate
				    '(lambda (f) (buffer-file-name (frame-get-buffer f)))))
	  (frames		(->> (frame-get-order)
					(-filter predicate)
					(reverse))))

    (when frames
	 (-each frames #'frame-banish)
	 (frame-position (-first-item frames) position)
	 (--each (-drop 1 frames)
	   (setq position (frame-cascade it position offset))))))


;; ---------- Commands ---------- ;;
(defun frame-cascade-file-buffers ()
  "Cascades all file buffer frames from the left corner."
  (interactive)
  (frame-cascade-frames))

(defun frame-cascade-project-buffers ()
  (interactive)
  nil)


;; ------------------------------------------------------------------------- ;;
;; Display Buffers Functions
;; ------------------------------------------------------------------------- ;;
(defun frame-display-buffers (&optional buffer-predicate)
  "Displays all buffers based on BUFFER-PREDICATE.

Specifically this function calls `display-buffer' on all buffers satisfying
BUFFER-PREDICATE and do not currently have a frame. If BUFFER-PREDICATE is nil,
file buffers will be used."
  (interactive)
  (let*
	 ((pred		(or buffer-predicate
				    (lambda (b) (buffer-file-name b))))
	  (buffers	(->> (buffer-list)
					(-filter pred)
					(-remove #'frame-get))))
    (-each buffers #'display-buffer)))

(defun frame--extend-display-buffer-action (action buffer alist)
  "Extends a display buffer ACTION to incorporate additional functionality.

The method calls ACTION for BUFFER with ALIST returning WINDOW if
successful. Prior to exiting however, applies the following if relevant
parameters are set:
	- Fit frame to buffer
	- Cascade frame from last"
  (-let*
	 (;; Get current frame count
	  (frame-created	(length (frame-list)))

	  ;; Executes the action
	  (window           (funcall action buffer alist))
	  
	  ;; Get any parameters we need
	  ((&alist
	    'cascade                        cascade

        'font-size                      font-size
        
	    'pop-up-frame-parameters
	    (&alist
		'fit-frame-always               fit-always
		'fit-frame-to-buffer-sizes      fit-sizes
		'fit-frame-to-buffer-margins	fit-margins))
	   alist))
    
    (when window
      (setq frame-created (> (length (frame-list)) frame-created))

      ;; Change Font Size
      (when font-size
        (set-face-attribute 'default (frame-get window) :height font-size))      

	  ;; Fit Frame
	  (when (and
		     ;; Require one of these
		     (or fit-sizes fit-margins)
		     ;; ...and either always fit OR this is a new frame
		     (or fit-always frame-created))	   
	    (frame-fit-to-buffer window))

	  ;; Cascade Frame
	  (when (and cascade frame-created)
	    (frame-cascade (frame-get window) (frame-position (frame-get)))))
    
    window))


;; Wrap each DISPLAY-BUFFER function
;; ------------------------------------------------------
;; LAST		-->		in-previous-window
(advice-add 'display-buffer-in-previous-window
		  :around #'frame--extend-display-buffer-action)

;; EXISTING	-->		reuse-window
(advice-add 'display-buffer-reuse-window
		  :around #'frame--extend-display-buffer-action)

;; THIS WINDOW	-->		same-window
(advice-add 'display-buffer-same-window
		  :around #'frame--extend-display-buffer-action)

;; NEW WINDOW	-->		pop-up-window
(advice-add 'display-buffer-pop-up-window
		  :around #'frame--extend-display-buffer-action)

;; ANY WINDOW	-->		use-some-window
(advice-add 'display-buffer-use-some-window
		  :around #'frame--extend-display-buffer-action)

;; NEW FRAME	-->		pop-up-frame
(advice-add 'display-buffer-pop-up-frame
		  :around #'frame--extend-display-buffer-action)

;; ANY FRAME	-->		use-some-frame
(advice-add 'display-buffer-use-some-frame
		  :around #'frame--extend-display-buffer-action)




(provide 'frame+)
;;; FRAME+.EL ends here

(require 'popup)
(require 'pos-tip)

(defun pos-tip-show
  (string &optional tip-color pos window timeout width frame-coordinates dx dy)
  "Show STRING in a tooltip, which is a small X window, at POS in WINDOW
using frame's default font with TIP-COLOR.

Return pixel position of tooltip relative to top left corner of frame as
a cons cell like (X . Y).

TIP-COLOR is a face or a cons cell like (FOREGROUND-COLOR . BACKGROUND-COLOR)
used to specify *only* foreground-color and background-color of tooltip.
If omitted, use `pos-tip-foreground-color' and `pos-tip-background-color'
instead.

Omitting POS and WINDOW means use current position and selected window,
respectively.

Automatically hide the tooltip after TIMEOUT seconds. Omitting TIMEOUT means
use the default timeout of 5 seconds. Non-positive TIMEOUT means don't hide
tooltip automatically.

WIDTH, if non-nil, specifies the width of filling each paragraph.

If FRAME-COORDINATES is omitted or nil, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the absolute coordinates
of the tooltip. If a cons cell like (LEFT . TOP), specifies the frame
absolute location and makes the calculation slightly faster, but can be
used only when it's clear that frame is in the specified position. Users
can get the latest values of frame coordinates for using in the next call
by referring the variable `pos-tip-saved-frame-coordinates' just after
calling this function. Otherwise, FRAME-COORDINATES `relative' means use
the pixel coordinates relative to the top left corner of the frame for
displaying the tooltip. This is the same effect as
`pos-tip-use-relative-coordinates' is non-nil.

DX specifies horizontal offset in pixel.

DY specifies vertical offset in pixel. This makes the calculations done
without considering the height of object at POS, so the object might be
hidden by the tooltip.

See also `pos-tip-show-no-propertize'.

JPH - Now uses `popup-tip-face'
"
  (unless window
    (setq window (selected-window)))
  (let* ((frame (window-frame window))
	 (max-width (pos-tip-x-display-width frame))
	 (max-height (pos-tip-x-display-height frame))
	 (w-h (pos-tip-string-width-height string)))
    (cond
     ((and width
	   (> (car w-h) width))
      (setq string (pos-tip-fill-string string width nil 'none nil max-height)
	    w-h (pos-tip-string-width-height string)))
     ((or (> (car w-h) max-width)
	  (> (cdr w-h) max-height))
      (setq string (pos-tip-truncate-string string max-width max-height)
	    w-h (pos-tip-string-width-height string))))
    ;; (face-spec-reset-face 'pos-tip-temp)
    ;; (with-selected-window window
    ;;   (set-face-font 'pos-tip-temp (frame-parameter frame 'font)))
    (pos-tip-show-no-propertize
     (propertize string 'face 'popup-tip-face) ;JPH
     tip-color pos window timeout
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
     frame-coordinates dx dy)))


(provide 'my-pos-tip)

;;;
;;; my-pos-tip.el ends Here
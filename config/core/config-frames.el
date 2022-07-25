;; -----------------------------------------------------------------------------
;; CONFIG-FRAMES.EL --- Custom Multi-Frame Setup
;; -----------------------------------------------------------------------------
(require 'frame+)

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------

(defalias 'find-file 'find-file-other-frame)

;; -------------------- Default Frame ------------------
;; TO-DO: set height depending on display size
(setq default-frame-alist
	 '((left . 0)
	   (top . 0)
	   (width . 81)
	   (height . 0.75)
	   (left-fringe . 0)
	   (right-fringe . 0)
	   (menu-bar-lines . 0)
	   (horizontal-scroll-bars . nil)
	   (vertical-scroll-bars . nil)))

;; Sets title bar as "<buffer>"
;;      Note: Emacs sets it to this if there are multiple frames anyways
(setq
 frame-title-format		"%b"
 frame-resize-pixelwise	1
 minibuffer-auto-raise	nil			; <- if true, raises on hover
 )

(customize-set-variable
 'display-buffer-base-action
 '((display-buffer-reuse-window
	display-buffer-pop-up-frame)
   (reusable-frames . 0)
   (dedicated . 1)
   (cascade . t)))

(customize-set-variable
 'display-buffer-alist
 `(("\\*Help.*\\*"
    (display-buffer-reuse-window
	display-buffer-pop-up-frame)
    (reusable-frames . 0)
    (cascade . nil)
    (dedicated . 1)
    (window-parameters
	.
	((mode-line-format . none)))
    (pop-up-frame-parameters
	.
	((unsplittable . t)
	 (horizontal-scroll-bars . nil)
	 (vertical-scroll-bars . nil)
	 (width . 60)
	 (top . 10)
	 (left . 790)
	 (minibuffer . nil)
	 (fit-frame-to-buffer-sizes . (30 0 100 0))
	 (fit-frame-always . t)
	 )))))


(provide 'config-frames)
;;; CONFIG-FRAMES.EL ends here


;; `display-buffer-overriding-action' > `display-buffer' >
;; > `display-buffer-base-action' > `display-buffer-fallback-action'

;; `display-buffer-fallback-action'
;;	- used if display-buffer or base-action is null (do not set)

;; `display-buffer-base-action'
;;	- default alist



;; `default-frame-alist'
;; `initial-frame-alist' - values specific to the first Emacs frame
;; `window-system-default-frame-alist' window-system specific values 

;; `switch-to-buffer' and `pop-to-buffer' use `display-buffer'

;; ACTION
;; BUFFER, ALIST --> return a window displaying buffer (success) or nil (fail)

;; Function: display-buffer-pop-up-frame buffer alist
;; This function creates a new frame, and displays the buffer in that frame’s window. It actually performs the frame creation by calling the function specified in pop-up-frame-function (see Choosing Window Options). If alist contains a pop-up-frame-parameters entry, the associated value is added to the newly created frame’s parameters.



;; FRAME-ALIST


;; WINDOW FRAME PARAMETERS

;; fit-frame-to-buffer-margins
;; fit-frame-to-buffer-sizes




;; default-minibuffer-frame



;; CONDITION is either a regexp matching buffer names, or a
;;  function that takes two arguments - a buffer name and the
;;  ACTION argument of ‘display-buffer’ - and returns a boolean.

;; ACTION is a cons cell (FUNCTIONS . ALIST), where FUNCTIONS is an
;;  action function or a list of action functions and ALIST is an
;;  action alist.  Each such action function should accept two
;;  arguments: a buffer to display and an alist of the same form as
;;  ALIST.  See ‘display-buffer’ for details.

;; ACTION = (FUNCTIONS . ALIST)

;; Action functions and the action they try to perform are:
;;  ‘display-buffer-same-window’ -- Use the selected window.
;;  ‘display-buffer-reuse-window’ -- Use a window already showing
;;     the buffer.
;;  ‘display-buffer-in-previous-window’ -- Use a window that did
;;     show the buffer before.
;;  ‘display-buffer-use-some-window’ -- Use some existing window.
;;  ‘display-buffer-pop-up-window’ -- Pop up a new window.
;;  ‘display-buffer-below-selected’ -- Use or pop up a window below
;;     the selected one.
;;  ‘display-buffer-at-bottom’ -- Use or pop up a window at the
;;     bottom of the selected frame.
;;  ‘display-buffer-pop-up-frame’ -- Show the buffer on a new frame.
;;  ‘display-buffer-in-child-frame’ -- Show the buffer in a
;;     child frame.
;;  ‘display-buffer-no-window’ -- Do not display the buffer and
;;     have ‘display-buffer’ return nil immediately.

;; Action alist entries are:
;;  ‘inhibit-same-window’ -- A non-nil value prevents the same
;;     window from being used for display.
;;  ‘inhibit-switch-frame’ -- A non-nil value prevents any frame
;;     used for showing the buffer from being raised or selected.
;;  ‘reusable-frames’ -- The value specifies the set of frames to
;;     search for a window that already displays the buffer.
;;     Possible values are nil (the selected frame), t (any live
;;     frame), visible (any visible frame), 0 (any visible or
;;     iconified frame) or an existing live frame.
;;  ‘pop-up-frame-parameters’ -- The value specifies an alist of
;;     frame parameters to give a new frame, if one is created.
;;  ‘window-height’ -- The value specifies the desired height of the
;;     window chosen and is either an integer (the total height of
;;     the window), a floating point number (the fraction of its
;;     total height with respect to the total height of the frame’s
;;     root window) or a function to be called with one argument -
;;     the chosen window.  The function is supposed to adjust the
;;     height of the window; its return value is ignored.  Suitable
;;     functions are ‘shrink-window-if-larger-than-buffer’ and
;;     ‘fit-window-to-buffer’.
;;  ‘window-width’ -- The value specifies the desired width of the
;;     window chosen and is either an integer (the total width of
;;     the window), a floating point number (the fraction of its
;;     total width with respect to the width of the frame’s root
;;     window) or a function to be called with one argument - the
;;     chosen window.  The function is supposed to adjust the width
;;     of the window; its return value is ignored.
;;  ‘preserve-size’ -- The value should be either (t . nil) to
;;     preserve the width of the chosen window, (nil . t) to
;;     preserve its height or (t . t) to preserve its height and
;;     width in future changes of the window configuration.
;;  ‘window-parameters’ -- The value specifies an alist of window
;;     parameters to give the chosen window.
;;  ‘allow-no-window’ -- A non-nil value means that ‘display-buffer’
;;     may not display the buffer and return nil immediately.


;; 29.13.3 Action Alists for Buffer Display
;; An action alist is an association list mapping predefined symbols recognized by action functions to values these functions are supposed to interpret accordingly. In each call, display-buffer constructs a new, possibly empty action alist and passes that entire list on to any action function it calls.

;; By design, action functions are free in their interpretation of action alist entries. In fact, some entries like allow-no-window or previous-window have a meaning only for one or a few action functions, and are ignored by the rest. Other entries, like inhibit-same-window or window-parameters, are supposed to be respected by most action functions, including those provided by application programs and external packages.

;; In the previous subsection we have described in detail how individual action functions interpret the action alist entries they care about. Here we give a reference list of all known action alist entries according to their symbols, together with their values and action functions (see Action Functions for Buffer Display) that recognize them. Throughout this list, the terms “buffer” will refer to the buffer display-buffer is supposed to display, and “value” refers to the entry’s value.

;; inhibit-same-window
;; If the value is non-nil, this signals that the selected window must not be used for displaying the buffer. All action functions that (re-)use an existing window should respect this entry.

;; previous-window
;; The value must specify a window that may have displayed the buffer previously. display-buffer-in-previous-window will give preference to such a window provided it is still live and not dedicated to another buffer.

;; mode
;; The value is either a major mode or a list of major modes. display-buffer-reuse-mode-window may reuse a window whenever the value specified by this entry matches the major mode of that window’s buffer. Other action functions ignore such entries.

;; frame-predicate
;; The value must be a function taking one argument (a frame), supposed to return non-nil if that frame is a candidate for displaying the buffer. This entry is used by display-buffer-use-some-frame.

;; reusable-frames
;; The value specifies the set of frames to search for a window that can be reused because it already displays the buffer. It can be set as follows:

;; nil means consider only windows on the selected frame. (Actually, the last frame used that is not a minibuffer-only frame.)
;; t means consider windows on all frames.
;; visible means consider windows on all visible frames.
;; 0 means consider windows on all visible or iconified frames.
;; A frame means consider windows on that frame only.
;; Note that the meaning of nil differs slightly from that of the all-frames argument to next-window (see Cyclic Ordering of Windows).

;; A major client of this is display-buffer-reuse-window, but all other action functions that try to reuse a window are affected as well. display-buffer-in-previous-window consults it when searching for a window that previously displayed the buffer on another frame.

;; inhibit-switch-frame
;; A non-nil value prevents another frame from being raised or selected, if the window chosen by display-buffer is displayed there. Primarily affected by this are display-buffer-use-some-frame and display-buffer-reuse-window. Ideally, display-buffer-pop-up-frame should be affected as well, but there is no guarantee that the window manager will comply.

;; window-parameters
;; The value specifies an alist of window parameters to give the chosen window. All action functions that choose a window should process this entry.

;; window-min-height
;; The value specifies a minimum height of the window used, in lines. If a window is not or cannot be made as high as specified by this entry, the window is not considered for use. The only client of this entry is presently display-buffer-below-selected.

;; Note that providing such an entry alone does not necessarily make the window as tall as specified by its value. To actually resize an existing window or make a new window as tall as specified by that value, a window-height entry specifying that value should be provided as well. Such a window-height entry can, however, specify a completely different value or ask the window height to be fit to that of its buffer in which case the window-min-height entry provides the guaranteed minimum height of the window used.

;; window-height
;; The value specifies whether and how to adjust the height of the chosen window and can be one of the following:

;; nil means to leave the height of the chosen window alone.
;; An integer number specifies the desired total height of the chosen window in lines.
;; A floating-point number specifies the fraction of the chosen window’s desired total height with respect to the total height of its frame’s root window.
;; If the value specifies a function, that function is called with one argument—the chosen window. The function is supposed to adjust the height of the window; its return value is ignored. Suitable functions are fit-window-to-buffer and shrink-window-if-larger-than-buffer, see Resizing Windows.
;; By convention, the height of the chosen window is adjusted only if the window is part of a vertical combination (see Windows and Frames) to avoid changing the height of other, unrelated windows. Also, this entry should be processed only under certain conditions which are specified right below this list.

;; window-width
;; This entry is similar to the window-height entry described before, but used to adjust the chosen window’s width instead. The value can be one of the following:

;; nil means to leave the width of the chosen window alone.
;; An integer specifies the desired total width of the chosen window in columns.
;; A floating-point number specifies the fraction of the chosen window’s desired total width with respect to the total width of the frame’s root window.
;; If the value specifies a function, that function is called with one argument—the chosen window. The function is supposed to adjust the width of the window; its return value is ignored.
;; By convention, the width of the chosen window is adjusted only if the window is part of a horizontal combination (see Windows and Frames) to avoid changing the width of other, unrelated windows. Also, this entry should be processed under only certain conditions which are specified right below this list.

;; dedicated
;; If non-nil, such an entry tells display-buffer to mark any window it creates as dedicated to its buffer (see Dedicated Windows). It does that by calling set-window-dedicated-p with the chosen window as first argument and the entry’s value as second. Side windows are by default dedicated with the value side ((see Side Window Options and Functions).

;; preserve-size
;; If non-nil such an entry tells Emacs to preserve the size of the window chosen (see Preserving Window Sizes). The value should be either (t . nil) to preserve the width of the window, (nil . t) to preserve its height or (t . t) to preserve both, its width and its height. This entry should be processed only under certain conditions which are specified right after this list.

;; pop-up-frame-parameters
;; The value specifies an alist of frame parameters to give a new frame, if one is created. display-buffer-pop-up-frame is its one and only addressee.

;; parent-frame
;; The value specifies the parent frame to be used when the buffer is displayed on a child frame. This entry is used only by display-buffer-in-child-frame.

;; child-frame-parameters
;; The value specifies an alist of frame parameters to use when the buffer is displayed on a child frame. This entry is used only by display-buffer-in-child-frame.

;; side
;; The value denotes the side of the frame or window where a new window displaying the buffer shall be created. This entry is used by display-buffer-in-side-window to indicate the side of the frame where a new side window shall be placed (see Displaying Buffers in Side Windows). It is also used by display-buffer-in-atom-window to indicate the side of an existing window where the new window shall be located (see Atomic Windows).

;; slot
;; If non-nil, the value specifies the slot of the side window supposed to display the buffer. This entry is used only by display-buffer-in-side-window.

;; direction
;; The value specifies a direction which, together with a window entry, allows display-buffer-in-direction to determine the location of the window to display the buffer.

;; window
;; The value specifies a window that is in some way related to the window chosen by display-buffer. This entry is currently used by display-buffer-in-atom-window to indicate the window on whose side the new window shall be created. It is also used by display-buffer-in-direction to specify the reference window on whose side the resulting window shall appear.

;; allow-no-window
;; If the value is non-nil, display-buffer does not necessarily have to display the buffer and the caller is prepared to accept that. This entry is not intended for user customizations, since there is no guarantee that an arbitrary caller of display-buffer will be able to handle the case that no window will display the buffer. display-buffer-no-window is the only action function that cares about this entry.

;; body-function
;; The value must be a function taking one argument (a displayed window). This function can be used to fill the displayed window’s body with some contents that might depend on dimensions of the displayed window. It is called after the buffer is displayed, and before the entries window-height, window-width and preserve-size are applied that could resize the window to fit it to the inserted contents.

;; By convention, the entries window-height, window-width and preserve-size are applied after the chosen window’s buffer has been set up and if and only if that window never showed another buffer before. More precisely, the latter means that the window must have been either created by the current display-buffer call or the window was created earlier by display-buffer to show the buffer and never was used to show another buffer until it was reused by the current invocation of display-buffer.

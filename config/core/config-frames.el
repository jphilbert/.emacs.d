;; -----------------------------------------------------------------------------
;; CONFIG-FRAMES.EL --- Custom Multi-Frame Setup
;; -----------------------------------------------------------------------------

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

;; CONDITION is either a regexp matching buffer names, or a
;;  function that takes two arguments - a buffer name and the
;;  ACTION argument of ‘display-buffer’ - and returns a boolean.

;; ACTION is a cons cell (FUNCTIONS . ALIST), where FUNCTIONS is an
;;  action function or a list of action functions and ALIST is an
;;  action alist.  Each such action function should accept two
;;  arguments: a buffer to display and an alist of the same form as
;;  ALIST.  See ‘display-buffer’ for details.

;; ACTION = (FUNCTIONS . ALIST)
(require 'frame+)

(defalias 'find-file 'find-file-other-frame)

(setq
 ;; Sets title bar as "<buffer>"
 frame-title-format             "%b"
 frame-resize-pixelwise         1
 ;; Turn off auto raising of minibuffer 
 minibuffer-auto-raise          nil 

 ;; -------------------- Default Frame ------------------
 ;; TODO: set height depending on display size
 default-frame-alist
 '((left .                      0)
   (top .                       0)
   (width .                     81)
   (height .                    0.75)
   (left-fringe .               0)
   (right-fringe .              0)
   (menu-bar-lines .            0)
   (horizontal-scroll-bars .    nil)
   (vertical-scroll-bars .      nil))
 
 display-buffer-base-action
 '((display-buffer-reuse-window display-buffer-pop-up-frame)
   (reusable-frames .          0)
   (dedicated .                1)
   (cascade .                  t))

 ;; -------------------- Special Frames ------------------
 display-buffer-alist
      `(("\\*Help.*\\*"
         (display-buffer-reuse-window display-buffer-pop-up-frame)
         (cascade .                 nil)

         (window-parameters
	      .
	      ((mode-line-format .      none)))
         
         (pop-up-frame-parameters
	      .
	      ((width .                 60)
	       (top .                   10)
	       (left .                  790)
           (unsplittable .          t)
	       (minibuffer .            nil)
	       (fit-frame-to-buffer-sizes
            .                       (30 0 100 0))
	       (fit-frame-always .      t)
	       )))))


;; (set-frame-parameter (frame-get "*Help*") 'minibuffer
;;                      (minibuffer-window (frame-get "config-frame.el")))



(provide 'config-frames)
;;; CONFIG-FRAMES.EL ends here

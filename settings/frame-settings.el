(provide 'frame-settings)


(setq-default fit-frame-max-width Multi-Window-Default-Window-Width)

(defvar Multi-Window-Terminal-Window-Top
  5
  "Default terminal window top position")
(defvar Multi-Window-Terminal-Window-Left
  (+ (x-display-pixel-width) 1)
    "Default terminal window left position")


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
             `(top . ,(+ 40 Multi-Window-Terminal-Window-Top))
	     `(left . ,(+ 40 Multi-Window-Terminal-Window-Left)))))

(add-to-list
 'special-display-regexps
 (list ".*\\*PowerShell.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 40)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 60)))))

;; -------------------- R Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*R.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 10)
             (cons 'left (+ (/ (x-display-pixel-width) 2) 20)))))

;; -------------------- Message Frame ---------------------
(add-to-list
 'special-display-buffer-names
 (list "*Messages*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 30)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 0)))))


;; -------------------- Backtrace Frame --------------------
(add-to-list
 'special-display-buffer-names
 (list "*Backtrace*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 30)
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 50)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 0)))))


;; -------------------- Python Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*python.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)             
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 10)
             (cons 'left (+ (/ (x-display-pixel-width) 2) 20)))))


;; -------------------- SQL Frame ---------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*SQL.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             `(height . ,Multi-Window-Default-Window-Height)
             `(width . ,Multi-Window-Default-Window-Width)
             '(top . 20)                ; Offsets the Terminals
             (cons 'left (+ (/ (x-display-pixel-width) 2) 40)))))



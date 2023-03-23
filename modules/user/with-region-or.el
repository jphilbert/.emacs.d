;; -------------------------------------------------------------------------- ;;
;; Applies DO-FUNC to an active region. If one is not active, uses OTHER-FUNC ;;
;; to select one. This is the general case of all subsequent functions.       ;;
;; -------------------------------------------------------------------------- ;;
(defun with-region-or- (do-func other-func &rest pass-thru-args)
  "Apply a command to a region or something else.

Take a command, DO-FUNC, that requires two arguments (begin and
end points) and applies it to the active region or some range
defined by OTHER-FUNC. OTHER-FUNC is either a list of points
defining the range or a function returning a list of points. Any
additional arguments given are passed to DO-FUNC.
"
  (setq other-func
        (cond ((use-region-p)
               (list (region-beginning) (region-end)))
              ((functionp other-func)
               (funcall other-func))
              ((listp other-func) other-func)))
  (apply do-func (append other-func pass-thru-args)))


;; -------------------------------------------------------------------------- ;;
;; Specific Wrappers                                                          ;;
;; -------------------------------------------------------------------------- ;;
(defun with-region-or-line (do-func &rest args)
  "Apply a command to a region or line."
  (with-region-or-
   do-func
   (list (line-beginning-position) (line-beginning-position 2))))

(defun with-region-or-eol (do-func &rest args)
  "Apply a command to a region or to the end of the line."
  (with-region-or-
   do-func
   (list (point) (line-end-position))))

(defun with-region-or-paragraph (do-func &rest args)
  "Apply a command to a region or to current paragraph."
  (with-region-or-
   do-func
   (lambda ()
     (save-excursion
       (end-of-paragraph-text)
       (let ((pt1 (point)))
         (start-of-paragraph-text)
         (list (point) pt1))))))

(defun with-region-or-buffer (do-func &rest args)
  "Apply a command to a region or buffer."
  (with-region-or-
   do-func
   (list (point-min) (point-max))))


;; -------------------------------------------------------------------------- ;;
;; Advice Macros                                                              ;;
;; -------------------------------------------------------------------------- ;;
(defmacro advise-with-region-or-line (do-func)
  "Advise DO-FUNC with `with-region-or-line'."
  `(advice-add ,do-func :around #'with-region-or-line))

(defmacro advise-with-region-or-eol (do-func)
  "Advise DO-FUNC with `with-region-or-eol'."
  `(advice-add ,do-func :around #'with-region-or-eol))

(defmacro advise-with-region-or-paragraph (do-func)
  "Advise DO-FUNC with `with-region-or-paragraph'."
  `(advice-add ,do-func :around #'with-region-or-paragraph))

(defmacro advise-with-region-or-buffer (do-func)
  "Advise DO-FUNC with `with-region-or-buffer'."
  `(advice-add ,do-func :around #'with-region-or-buffer))


(provide 'with-region-or)
;;; with-region-or.el ends here


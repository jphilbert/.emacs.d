(require 'dash) 
(require 'cl-lib)


(defalias '-mapcar 'cl-mapcar)

(defun -pad-n (fill-value n &optional list)
  "Appends FILL-VALUE to or drops elements from the end of LIST such that it have length N."
  (-take n (append (-list list) (-repeat (- n (length list)) fill-value))))

(defun -to-cons (list)
  (append (-drop-last 1 list) (-last-item list)))

(defalias '-to-list 'flatten-tree)

(defun -query-plist (plist &rest keys)
  (-reduce-from 'plist-get plist keys))

(provide 'dash+)
;;; DASH+.EL ends here


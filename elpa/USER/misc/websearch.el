;;; websearch.el --- builds interfaces to a bunch of searchers with customizable keybindings

;; Copyright (C) 2009, 2010 tkykhs

;; Author:     tkykhs <tkykhs@gmail.com>
;; Maintainer: tkykhs
;; Keywords:   search

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To install websearch.el, place it under your load-path and then insert the
;; following line into your .emacs.
;; 
;;  (require 'websearch)
;; 
;; Default keybindings:
;; 
;;   \M-s SPACE =  websearch-show-query-popup-menu
;;   \M-s w     =  websearch-wikipedia-en
;;   \M-s g     =  websearch-google
;;   \M-s c     =  websearch-google-code
;;   \M-s m     =  websearch-google-maps
;;   \M-s Y     =  websearch-yahoo
;;   \M-s t     =  websearch-twitter
;;   \M-s a     =  websearch-amazon
;;   \M-s A     =  websearch-amazon-book
;;   \M-s y     =  websearch-youtube
;;
;; Custom Searchers:
;; 
;; Any searcher addition/customization should be done with the
;; `customize-variable' interface like so:
;; 
;;  (customize-variable 'websearch-entries)
;; 
;; Every entry in `websearch-entries' is bound to become a searcher.
;; For example, (google "http://www.google.com/search?q=" "Google: " utf-8 "g")
;; defines:
;; 
;;   * `websearch-google' command, which you use to search for any query,
;;     which, if given, will be encoded in utf-8.
;;
;;   * key-mapping:  \M-s g  =  websearch-google
;; 
;; Here, you can change the default key-prefix by changing the value of
;; `websearch-key-prefix'.
;; 
;; If you add a new websearch entry through the `customize-variable'
;; interface, a searcher corresponding to it will become available as soon as
;; you save `websearch-entries'.

;;; Todo:

;;; Code:

(defgroup websearch nil
  "Web-searcher builder in Emacs."
  :prefix "websearch-"
  :group 'websearch)

(defcustom websearch-key-prefix "\M-s"
  "Prefix key to use for searchers defined in `websearch-entries'.
The value of this variable only matters when `websearch-define-global-keymap'
is non-nil."
  :type  'sexp
  :group 'websearch)

(defcustom websearch-define-global-keymap t
  "Whether to define a global keymap for searchers prefixed with \
`websearch-key-prefix' or not."
  :type  'boolean
  :group 'websearch)

(defcustom websearch-browse-url-function 'browse-url
  "Function for browsing search result pages."
  :type  'function
  :group 'websearch)

(defcustom websearch-max-popup-queries 20
  "Maximum number of queries in history to show in the popup menu."
  :type  'integer
  :group 'websearch)

(defcustom websearch-last-query-persistent-p t
  "Whether to insert the last query in the minibuffer as the default value."
  :type  'boolean
  :group 'websearch)

(defcustom websearch-modify-query-p t
  "Whether to modify the region content before sending a query."
  :type  'boolean
  :group 'websearch)


(defconst websearch-function-prefix 'websearch
  "Symbol prefix that gets prepended to <searcher-symbol>.
Generated symbol name for the searcher function is of the form
`<websearch-function-prefix>-<searcher-symbol>'.")

(defvar websearch-query-history nil
  "History list of queries used in websearch searchers")


(when websearch-define-global-keymap
  (defvar websearch-prefix-map (make-sparse-keymap))
  (fset 'websearch-prefix-map websearch-prefix-map)
  (global-unset-key websearch-key-prefix)
  (global-set-key websearch-key-prefix websearch-prefix-map)
  ;; Prevent any `major-mode' from stealing the key prefixwe just defined.
  (add-hook 'after-change-major-mode-hook
            (lambda () (local-set-key websearch-key-prefix 'websearch-prefix-map))))


(defun websearch-url-encode (s &optional encoding space-to-plus-p)
  (mapconcat
   (lambda (ch)
     (cond ((eq ch ?\n) "%0D%0A")
           ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch))
            (char-to-string ch))
           ((char-equal ch ?\x20)
            (if space-to-plus-p "+" "%20"))
           (t (format "%%%02X" ch))))
   (string-to-list (encode-coding-string (or s "") (or encoding 'utf-8)))
   nil))

(defun websearch-build-searcher (websearch-entry)
  (let ((name (intern (format "%S-%S"
                              websearch-function-prefix
                              (car websearch-entry))))
        (key (nth 4 websearch-entry)))
    (setf (symbol-function name)
          (lexical-let ((url            (nth 1 websearch-entry))
                        (minibuf-string (nth 2 websearch-entry))
                        (encoding       (nth 3 websearch-entry)))
            #'(lambda (&optional query)
                (interactive)
                (websearch-query url minibuf-string encoding query))))
    (when (and key websearch-define-global-keymap)
      (define-key websearch-prefix-map key (symbol-function name)))))


(defsubst websearch-trim (s)
  (replace-regexp-in-string
   "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" s))


(defun websearch-query (url minibuffer-string encoding &optional query)
  (unless query
    (let ((region-string (and (region-active-p)
                              (prog1
                                  (websearch-trim
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))
                                (deactivate-mark)))))
      (setq query
            (or (and (not websearch-modify-query-p) region-string)
                (read-from-minibuffer
                 minibuffer-string
                 (or region-string
                     (if websearch-last-query-persistent-p
                         (car websearch-query-history)))
                 nil nil 'websearch-query-history)))))
  (funcall websearch-browse-url-function
           (format "%s%s" url (websearch-url-encode query encoding))))


;; (defun websearch-reload-entries ()
;;   (interactive)
;;   (dolist (entry websearch-entries)
;;     (websearch-build-searcher entry)))


;;;; Popup menu stuff.


(defun websearch-point-to-coord (&optional point)
  "Get the xoffset/yoffset information of POINT.
If POINT is not given, default is to current point.
If `posn-at-point' is not available (like in Emacs 21.3),
t is returned simply. This is borrowed from yasnippet.el."
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun websearch-strings-to-keymap (strings)
  (cons 'keymap
        (mapcar (lambda (s)
                  (if (listp s)
                      (list (car s) 'menu-item (car s)
                            (websearch-strings-to-keymap (nth 1 s)) t)
                    (list s 'menu-item s  t)))
                strings)))


(defun websearch-x-popup-list (strings)
  "Show a popup menu listing the given strings at cursor."
  (x-popup-menu (websearch-point-to-coord)
                (websearch-strings-to-keymap strings)))


(defun websearch-show-query-popup-menu ()
  "Show a popup menu of the query history and all available searchers at cursor.
If \"<query>  >  <searcher>\" is selected, another search will be
conducted with \(<searcher> <query>) function call. It shows no menu if
there is nothing in the query history."
  (interactive)
  (when (and window-system websearch-query-history)
    (let* ((searcher-list
            (mapcar (lambda (e) (format "%S" (nth 0 e))) websearch-entries))
           (menu (let (L)
                   (dotimes (i (min (length websearch-query-history) websearch-max-popup-queries))
                     (setq L (append L (list `(,(nth i websearch-query-history) ,searcher-list))))) L))
           (selected-entry (websearch-x-popup-list menu))
           (query (nth 0 selected-entry))
           (searcher-function (symbol-function
                               (intern (format "%S-%s"
                                               websearch-function-prefix
                                               (nth 1 selected-entry))))))
      (funcall searcher-function query))))


;;;; websearch-entries


;; Purposefully placed at the end.
(defcustom websearch-entries
  '((wikipedia-en "http://en.wikipedia.org/wiki/Search?search=" "Wikipedia (en) search: " utf-8 "w")
    (google       "http://www.google.com/search?q=" "Google: " utf-8 "g")
    (google-code  "http://www.google.com/codesearch?as_q=" "Google code: " utf-8 "c")
    (google-image "http://images.google.com/images?source=ig&hl=en&um=1&ie=UTF-8&sa=N&tab=wi&q=" "Google Image: " utf-8 "i")
    (google-maps  "http://maps.google.com/maps?q=" "Google Maps: " utf-8 "m")
    (yahoo        "http://search.yahoo.com/search?p=" "Yahoo!: " utf-8 "Y")
    (twitter      "http://search.twitter.com/search?q=" "Twitter search: " utf-8 "t")
    (amazon       "http://www.amazon.com/s?url=search-alias%3Daps&x=0&y=0&field-keywords=" "Amazon: " utf-8 "a")
    (amazon-book  "http://www.amazon.com/s/?url=index%3Dstripbooks%3Arelevance-above&Go.x=0&Go.y=0&Go=Go&keywords=" "Amazon Book: " utf-8 "A")
    (youtube      "http://www.youtube.com/results?search=Search&search_query=" "YouTube: " utf-8 "y"))    ;; search_sort=video_date_uploaded&
  "List of websearch entries.
Any searcher addition/customization should be done with the
`customize-variable' interface like so:

 \(customize-variable 'websearch-entries)

Every entry in `websearch-entries' is bound to become a searcher.
For example, (google \"http://www.google.com/search?q=\" \"Google: \" utf-8 \"g\")
defines:

  * `websearch-google' command, which you use to search for any query,
    which, if given, will be encoded in utf-8.

  * key-mapping:  \\M-s g  =  websearch-google

Here, you can change the default key-prefix by changing the value of
`websearch-key-prefix'.

If you add a new websearch entry through the `customize-variable'
interface, a searcher corresponding to it will become available as soon as
you save `websearch-entries'."
  :type '(repeat (list
                  (symbol :tag "Function postfix (symbol)")
                  (string :tag "URL except query")
                  (string :tag "Minibuffer string")
                  (symbol :tag "Encoding" :value utf-8)
                  (choice (const :tag "Key unspecified" nil)
                          (string :tag "Key following prefix"))))
  :set  (lambda (symbol value)
          (set-default symbol value)
          (dolist (entry value)
            (websearch-build-searcher entry)))
  :group 'websearch)

(define-key websearch-prefix-map " " 'websearch-show-query-popup-menu)

(provide 'websearch)
;;; websearch.el ends here

;; -----------------------------------------------------------------------------
;; SWITCH-FRAME.EL --- Simple cycling focus of frames
;; -----------------------------------------------------------------------------
;; Filename:		SWITCH-FRAME.EL
;; Description:		Simple cycling focus of frames
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Copyright (C)	2012, John P. Hilbert, all rights reserved.
;; Created:		2012-02-10 09:59:07
;; Version:		0.1
;; Last-Updated:	2014-04-06
;;           By:	John P. Hilbert
;; URL:		
;; Keywords:		<NONE>
;; Compatibility:	GNU Emacs 23.3.1 - 24.3
;;
;; Features that might be required by this library:
;; <NONE>

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
;; Simple cycling focus of frames.  As opposed to simply moving up or down the
;; frame stack, the primary functions (switch-frame-...-buffer) allow filtering
;; of buffer.  The filtering can be automatic via custom variables or passing of
;; include or exclude regexp lists.
;;

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put SWITCH-FRAME.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'switch-frame)
;;	

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------
(defcustom switch-frame-exclude-buffer-regexps '("\\*")
  "*List of regular expressions for excluded buffers. The default setting EXCLUDES buffers containing '*'."
  :type 'string
  :group 'switch-frame)

(defcustom switch-frame-include-buffer-regexps '(".*")
  "*List of regular expressions for including buffers. The default setting d INCLUDES all buffers."
  :type 'string
  :group 'switch-frame)
;;
;; All of the above can customize by:
;;      M-x customize-group RET switch-frame RET
;;

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2012/02/10
;;      * First released.
;; 2013/05/21
;;	* SWITCH-FRAME-XXX - SETS the current buffer in the end
;;	* added function END-OF-BUFFER-ALL function that moves the cursor to the
;;	  end for all windows
;; 2014-04-06
;;	* Moved non-package or unneeded functions (end-of-buffer-all,
;;	  save-frame-excursions)
;;	* Non-functional changes in some methods
;; 

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; Thanks to code written by David Ponce <david@dponce.com> that led me in the
;;  right direction.

;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
;; <NONE>

;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun switch-frame-exclude-p (name &optional exclude-list)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `switch-frame-exclude-buffer-regexps'."
  (let ((rl (or exclude-list
	      switch-frame-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun switch-frame-include-p (name &optional include-list)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `switch-frame-exclude-buffer-regexps'."
  (let ((rl (or include-list
	      switch-frame-include-buffer-regexps)))
    (while (and rl (string-match (car rl) name))
      (setq rl (cdr rl)))
    (null rl)))

(defun switch-frame-buffer-list (&optional include-list exclude-list)
  "Return the list of switchable buffers which match include-list and do not match exclude-list. NOTE: the list may include the current buffer."
  (delq nil
        (mapcar (function
                 (lambda (buf)
                   (and (switch-frame-include-p (buffer-name buf)
                                                include-list)
                        (switch-frame-exclude-p (buffer-name buf)
                                                exclude-list)
                        buf)))
                (buffer-list))))

(defun switch-frame-previous-buffer (&optional include-list exclude-list
					       set-active)
  "Display and activate the buffer at the end of the buffer list."
  (let ((l(delq (current-buffer)
                (switch-frame-buffer-list include-list exclude-list))))
    (when l
      (display-buffer-other-frame (first l))
      (when set-active
	(set-buffer (car (last l)))))))

(defun switch-frame-next-buffer (&optional include-list exclude-list
					   set-active)
  "Display and activate the next buffer in the buffer list."
  (let ((l (delq (current-buffer)
                 (switch-frame-buffer-list include-list exclude-list))))
    (when l
      (display-buffer-other-frame (car (last l)))
      (when set-active
	(set-buffer (car (last l)))))))


;; --------------------------------------------------------------------------
;; Common (interactive) Wrappers
;; --------------------------------------------------------------------------
(defun switch-frame-next ()
  (interactive)
  (switch-frame-next-buffer))

(defun switch-frame-previous ()
  (interactive)
  (switch-frame-previous-buffer))

(provide 'switch-frame)

;;; SWITCH-FRAME.EL ends here





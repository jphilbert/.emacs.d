;;; ac-anaconda.el --- Anaconda sources for auto-complete-mode

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/ac-anaconda
;; Version: 20140705.31
;; X-Original-Version: 0.1.0
;; Package-Requires: ((auto-complete "1.4.0") (anaconda-mode "0.1.0") (dash "2.6.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'auto-complete)
(require 'anaconda-mode)

(defun ac-anaconda-candidates ()
  "Obtain candidates list from anaconda."
  (--map (popup-make-item (plist-get it :name)
                          :document (plist-get it :doc)
                          :summary (plist-get it :type))
         (anaconda-mode-complete)))

(ac-define-source anaconda
  '((candidates . ac-anaconda-candidates)
    (symbol . "a")))

;;;###autoload
(defun ac-anaconda-setup ()
  "Set up `ac-sources' for `anaconda-mode'."
  (interactive)
  (push 'ac-source-anaconda ac-sources)
  (unless auto-complete-mode
    (auto-complete-mode)))

(provide 'ac-anaconda)

;;; ac-anaconda.el ends here

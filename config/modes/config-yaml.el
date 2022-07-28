;;; prelude-yaml.el --- Emacs Prelude: YAML programming support.
;;
;; Copyright © 2011-2021 Bozhidar Batsov
;;
;; Author: ToBeReplaced
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for YAML.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.

(defun config-mode-yaml ()
  "Default coding hook, useful with any programming language."
  (whitespace-mode)
  (subword-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'yaml-mode-hook 'config-mode-yaml)

(provide 'config-yaml)
;;; prelude-yaml.el ends here


;; (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
;;   (:map yaml-mode-map
;;        ("\C-m"   . newline-and-indent)))

;;; prelude-web.el --- Emacs Prelude: web template support
;;
;; Copyright © 2011-2021 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for web-mode.

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

(prelude-require-packages '(web-mode))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

(sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-tag "%" "<% "  " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

(with-eval-after-load 'web-mode
  (defun prelude-web-mode-defaults ())
  (setq prelude-web-mode-hook 'prelude-web-mode-defaults)

  (add-hook 'web-mode-hook (lambda ()
                             (run-hooks 'prelude-web-mode-hook))))

(provide 'prelude-web)
;;; prelude-web.el ends here

;; ----------------------------------------------------------------------------
;; Web Mode Setup
;; ----------------------------------------------------------------------------
(provide 'web-setup)
(require 'web-mode)
(require 'js-setup)

(setq-default web-mode-enable-current-element-highlight t)

(add-to-list				; Home
 'browse-url-filename-alist
 '("C:/Users/JPHil_000/Documents/WWW/" . "http://localhost/"))

(add-to-list				; Work
 'browse-url-filename-alist
 '("C:/Users/hilbertjp/Local_Files/WWW" . "http://localhost/"))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'html-mode-hook		'my-html-mode-hook)
(defun my-html-mode-hook ()
  (interactive)
  (flyspell-prog-mode)
  (web-mode)

  (setq-local comment-auto-fill-only-comments nil)
  (turn-on-auto-fill)

  (add-to-list 'ac-sources 'ac-source-css-property)
  (auto-complete-mode)
  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   [(f3)]               'web-mode-fold-or-unfold
   
   ;; ---------- Evaluation ----------
   [(shift return)]	'(lambda ()
			   (interactive)
			   (save-buffer)
			   (browse-url-of-buffer))

   ;; ---------- Movement ----------
   (kbd   "<M-left>") 		'web-mode-element-previous
   (kbd   "<M-right>")		'web-mode-element-next

   (kbd   "<M-down>")		'(lambda ()
				   (interactive)
				   (web-mode-element-end)
				   (web-mode-element-next))
   (kbd   "<M-up>")		'(lambda ()
				   (interactive)
				   (web-mode-tag-previous)
				   (web-mode-tag-match))


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "html "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "html "))
   (kbd "C-h e")   	'web-mode-errors-show
   ))


(add-hook 'css-mode-hook		'my-css-mode-hook)
(defun my-css-mode-hook ()
  (interactive)
  (skewer-css-mode)
  (flyspell-prog-mode)
  (hs-minor-mode)

  (turn-on-auto-fill)

  ;; ------------------------------------------------------
  ;; Key Binding
  ;; ------------------------------------------------------
  (local-set-many-keys
   ;; ---------- Evaluation ----------
   [(shift return)]	'css-eval
   [(M-S-return)]	'css-revert
   [(C-S-return)]	'css-eval-buffer
   
   ;; ---------- Completion ----------


   ;; ---------- Help ----------
   [(S-f1)]	   	'(lambda ()
			   (interactive)
			   (google-query-at-point t "CSS "))
   (kbd "C-h w")   	'(lambda ()
			   (interactive)
			   (google-query-at-point nil "CSS "))
   ))

;; --------------------------------------------------------------------------
;; Additional Functions
;; --------------------------------------------------------------------------
(defun tag-word-or-region (tag)
  "Surround current word or region with a given tag."
;; http://www.johndcook.com/blog/2010/08/05/emacs-command-to-add-html-tags/
    (interactive "sEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" tag ">"))
        (if (and transient-mark-mode mark-active)
            (progn
	      (setq pos2 (region-end)
		    pos1 (region-beginning))
                (goto-char pos2)
                (insert end-tag)
                (goto-char pos1)
                (insert start-tag)
		(goto-char (+ pos2 (string-width start-tag)
			      (string-width end-tag))))
	  (progn
	    (message "bye")
                (setq bds (bounds-of-thing-at-point 'symbol))
                (goto-char (cdr bds))
                (insert end-tag)
                 (goto-char (car bds))
                 (insert start-tag)
		 (goto-char (+ (cdr bds)
			       (string-width start-tag)
			       (string-width end-tag)))))))

(defun css-eval ()
  "Evaluates CSS code."
  (interactive)
  (if skewer-clients
      (progn
	(skewer-css-eval-current-rule)
	(goto-char (skewer-css-end-of-rule))
	(next-non-blank-line))
    (message "ERROR: No Skewer Clients")))

(defun css-revert ()
  "Reverts any evaluated CSS code."
  (interactive)
  (if skewer-clients
      (skewer-css-clear-all)
    (message "ERROR: No Skewer Clients")))

(defun css-eval-buffer ()
  "Evaluates all CSS buffer."
  (interactive)
  (if skewer-clients
      (skewer-css-eval-buffer)
    (message "ERROR: No Skewer Clients")))


;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting
;; ------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'js2-mode
 '(("[\\<-][0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("\\([<=>]=+\\|[!<>&|]\\)"
    .
    'font-lock-relation-operator-face)))

(font-lock-add-keywords
 'js-mode
 '(("[\\<-][0-9]*\\.?[0-9]+"
    .
    'font-lock-number-face)
   ("\\([<=>]=+\\|[!<>&|]\\)"
    .
    'font-lock-relation-operator-face)))



;;; web-setup.el ends here




;; ========================================================================== ;;
;; CONFIG-TEMPLATE.EL --- configuration for TEMPLATE                          ;;
;; ========================================================================== ;;
;; This file is not part of GNU Emacs.

;;; Code:
(require 'PACKAGES)

;; -------------------------------------------------------------------------- ;;
;; Hooks                                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defun config-mode-TEMPLATE ()
  "Defaults for TEMPLATE mode."
  )

(defun config-mode-TEMPLATE-interactive ()
  "Defaults for TEMPLATE interactive mode."
  )

(add-hook 'TEMPLATE-mode-hook               'config-mode-TEMPLATE)
(add-hook 'TEMPLATE-interactive-mode-hook   'config-mode-TEMPLATE-interactive)


;; -------------------------------------------------------------------------- ;;
;; Frame Settings                                                             ;;
;; -------------------------------------------------------------------------- ;;
(add-to-list
 'display-buffer-alist
 '("\\*REGEX FOR INTERACTIVE FRAME\\*"
   (display-buffer-reuse-window display-buffer-pop-up-frame)
   (cascade .                   nil)
   (font-size .                 100)

   (pop-up-frame-parameters
    .
    ((top .                     20)
	 (left .                    810) 
	 (height .                  0.6) 
     (unsplittable .            t)
	 ))))


;; -------------------------------------------------------------------------- ;;
;; Keybinding                                                                 ;;
;; -------------------------------------------------------------------------- ;;
(define-keys		TEMPLATE-mode-map
  ;; ---------- Help ----------
  "\C-hf"			'TEMPLATE-help-function
  "\C-hv"			'TEMPLATE-help-variable
  )


;; -------------------------------------------------------------------------- ;;
;; Syntax Highlighting                                                        ;;
;; -------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'TEMPLATE-mode        
 '(("\\_<[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?"
    .
    'font-lock-number-face)
   ("(\\([/!<>]=\\|[<>=]\\|and\\|or\\|not\\|equal\\|eq[1]?\\)\\_>"
    .
    'font-lock-relation-operator-face)))


;; -------------------------------------------------------------------------- ;;
;; Commands                                                                   ;;
;; -------------------------------------------------------------------------- ;;
(defun TEMPLATE-eval ()
  "Evaluate TEMPLATE code"
  )

(defun TEMPLATE-eval-insert ()
  "Evaluate TEMPLATE code and insert"
  )

(defun TEMPLATE-set-repl ()
  "Sets which REPL buffer to use for TEMPLATE"
  )

(defun TEMPLATE-create-repl ()
  "Creates a REPL buffer to use for TEMPLATE"
  )

(defun TEMPLATE-help-function ()
  "Show help for TEMPLATE function"
  )

(defun TEMPLATE-help-variable ()
  "Show help for TEMPLATE variable"
  )

;; -------------------------------------------------------------------------- ;;
;; Functions                                                                  ;;
;; -------------------------------------------------------------------------- ;;


(provide 'config-TEMPLATE)
;;; config-TEMPLATE.el ends here

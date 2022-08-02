;;; CONFIG-MODES.EL --- modes to load on startup

;; Mode File Inheritance
;; <Fundamental>
;; ├─ Text 
;; |  └─ Org
;; |
;; └─ Programming
;;    ├─ Lisp
;;    |  └─ EMACS-Lisp
;;    | 
;;    ├─ CSS
;;    ├─ Web
;;    ├─ HTML
;;    ├─ JS
;;    |
;;    ├─ XML
;;    ├─ YAML
;;    ├─ Init
;;    |
;;    ├─ Python
;;    ├─ R
;;    ├─ SQL
;;    |
;;    ├─ Shell
;;    └─ PowerShell
;;
;;   ComInt
;;    ielm = inferior-emacs-lisp-mode
;;    shell

(require 'config-text)
;; (require 'config-org)

;;; Programming Languages Support
(require 'config-programming)
(require 'config-lisp)
(require 'config-emacs-lisp)

;; (require 'config-web)
;; (require 'config-css)
;; (require 'config-js)

;; (require 'config-xml)
(require 'config-yaml)

(require 'config-python)
(require 'config-r)
(require 'config-sql)

;; (require 'config-shell)

(provide 'config-modes)
;;; CONFIG-MODES.EL ends here

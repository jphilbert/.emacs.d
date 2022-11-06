;;; CONFIG-MODES.EL --- modes to load on startup

;; Mode File Inheritance
;; <Fundamental>        (File)      (REPL)
;; ├─ Text 
;; |  └─ Org
;; |
;; └─ Programming                   COMINT
;;    |                 
;;    ├─ EMACS-Lisp     .el         <ielm>
;;    | 
;;    ├─ CSS            .css
;;    ├─ HTML           .html
;;    ├─ JS             .js
;;    |
;;    ├─ XML            .xml
;;    ├─ YAML           .yaml
;;    ├─ Config         .ini / .cfg
;;    |
;;    ├─ Python         .py         <Python>
;;    ├─ R              .r          <R>
;;    ├─ SQL            .sql        <SQL>
;;    |
;;    ├─ Shell          .bat        <DOS>
;;    └─ PowerShell     .ps         <Powershell>
;;

(require 'config-text)
;; (require 'config-org)

;;; Programming Languages Support
(require 'config-programming)


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

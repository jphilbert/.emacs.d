;;; CONFIG-MODES.EL --- modes to load on startup

;; ----- Configuration File ---- ;; -------- Mode File Inheritance ---------- ;;
;;                               ;; <Fundamental>       (File)  (REPL)
(require 'config-text)           ;; ├─ Text 
(require 'config-markdown)       ;; |  |  Markdown      .md
;; (require 'config-org)         ;; |  └─ Org
;;                               ;; |
(require 'config-programming)    ;; └─ Programming      .*      <comint>
;;                               ;;    |                 
(require 'config-emacs-lisp)     ;;    ├─ EMACS-Lisp    .el     <ielm>
;;                               ;;    | 
;; (require 'config-web)         ;;    ├─ CSS           .css
;; (require 'config-css)         ;;    ├─ HTML          .html
;; (require 'config-js)          ;;    ├─ JS            .js
;;                               ;;    |
;; (require 'config-xml)         ;;    ├─ XML           .xml
(require 'config-yaml)           ;;    ├─ YAML          .yaml
;;                               ;;    ├─ Config        .ini / .cfg
;;                               ;;    |
(require 'config-python)         ;;    ├─ Python        .py     <Python>
(require 'config-r)              ;;    ├─ R             .r      <R>
(require 'config-sql)            ;;    ├─ SQL           .sql    <SQL>
;;                               ;;    |
;; (require 'config-shell)       ;;    ├─ Shell         .bat    <DOS>
;;                               ;;    └─ PowerShell    .ps     <Powershell>


(provide 'config-modes)
;;; CONFIG-MODES.EL ends here

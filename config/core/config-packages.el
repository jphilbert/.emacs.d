;;; config-packages.el --- Emacs Config: default package selection.

;;;; Packages / Features: 
 ; Set first

(require 'cl-lib)
(require 'package)

;;; Code:
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; (package-refresh-contents t)

(defvar config-packages
  '(
    ;; ---------- Utilities ---------- ;;
    crux
    dash                        ; List functions
    f							; File functions
    s							; String functions
    smartrep

    ;; ---------- UI ---------- ;;
    rainbow-delimiters			; Colors delimiters
    zenburn-theme
    powerline					; Mode Line
;;    octicons

    ;; ---------- Minor ---------- ;;
    expand-region					
    operate-on-number			; Inline simple math
    which-key					; Help on keys when needed
    smartparens
    smart-hungry-delete
    hl-todo
    
    ;; ---------- Complete ---------- ;;
    vertico						; Minimalistic vertical completion UI
    consult						; Useful search and navigation commands
    orderless					; Advanced completion style
    marginalia					; Rich annotations in the minibuffer
    embark						; Minibuffer actions and context menu
    cape

    corfu
    tempel
    
    flyspell-correct
    flyspell-correct-popup

    ;; ---------- Tools ---------- ;;
    rg
    
    ;; ---------- Modes ---------- ;;
    markdown-mode
    ess
    yaml-mode
    sql-indent

    ;; ---------- Untested ---------- ;;
    ;; adaptive-wrap
    ;; all-the-icons
    ;; deferred
    ;; super-save
    ;; undo-tree
    ;; zop-to-char
    ;; magit
    ;; nlinum
    ;; epl
    ;; browse-kill-ring

    ;; projectile
    ;; move-text
    
    ;; diff-hl
    ;; easy-kill
    ;; gist
    ;; git-timemachine
    ;; git-modes
    ;; volatile-highlights
      )
  "A list of packages to ensure are installed at launch.")

(defun config-packages-installed-p ()
  "Check if all packages in `config-packages' are installed."
  (cl-every #'package-installed-p config-packages))

(defun config-require-package (package)
  "Install PACKAGE unless already installed."
  ;; Add package to list
  (unless (memq package config-packages)
    (add-to-list 'config-packages package))
  ;; Check if installed
  (unless (package-installed-p package)
    (package-install package)))

(defun config-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'config-require-package packages))

(defun config-install-packages ()
  "Install all packages listed in `config-packages'."
  (unless (config-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "[CONFIG] Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " ...done.")
    ;; install the missing packages
    (config-require-packages config-packages)))


;; (require 'epl)
;; (defun config-update-packages (&optional arg)
;;   "Update Config's packages.
;; This includes package installed via `config-require-package'.

;; With a prefix ARG updates all installed packages."
;;   (interactive "P")
;;   (when (y-or-n-p "Do you want to update Config's packages? ")
;;     (if arg
;;         (epl-upgrade)
;;       (epl-upgrade (cl-remove-if-not (lambda (p) (memq (epl-package-name p) config-packages))
;;                                      (epl-installed-packages))))
;;     (message "Update finished. Restart Emacs to complete the process.")))


;; run package installation
(config-install-packages)

(provide 'config-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; config-packages.el ends here

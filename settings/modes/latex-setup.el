;; ----------------------------------------------------------------------------
;; My LaTeX Setup
;; ----------------------------------------------------------------------------
(provide 'latex-setup)

(require 'tex-site)
(require 'preview-latex)

(setq TeX-PDF-mode t
      TeX-parse-self t
      TeX-master nil
      TeX-auto-save t
      preview-auto-cache-preamble t)

;; (setq preview-gs-command "C:\\Program Files (x86)\\gs\\gs9.00\\bin\\gswin32c.exe")

(add-hook 'TeX-mode-hook 'my-latex-mode-hook)
(defun my-latex-mode-hook ()
  (TeX-fold-mode 1)
  (flyspell-mode t)

  ;; --------------------------------------------------------------------------
  ;; Keybindings
  ;; --------------------------------------------------------------------------
  (global-set-key (kbd "(") 	'skeleton-pair-insert-maybe)
  (global-set-key (kbd "[")	'skeleton-pair-insert-maybe)
  (global-set-key (kbd "{")	'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\"")	'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\`")	'skeleton-pair-insert-maybe)
  )


;; ----------------------------------------------------------------------------
;; Sweave 
;; ----------------------------------------------------------------------------
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
;;(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))

(add-hook 'Rnw-mode-hook 'my-sweave-mode-hook)
(defun my-sweave-mode-hook ()
  (add-to-list 'TeX-expand-list
	       '("%rnw" file "Rnw" t) t)
  (add-to-list 'TeX-command-list
	       '("Stangle" "R CMD Stangle %rnw"
		 TeX-run-command nil (latex-mode) :help "Run Stangle") t)
  (add-to-list 'TeX-command-list
	       '("Sweave" "R CMD Sweave %rnw"
		 TeX-run-command nil (latex-mode) :help "Run Sweave") t)
  (add-to-list 'TeX-command-list
	       '("LatexSweave" "%l %(mode) %s"
		 TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
  (setq TeX-command-default "Sweave")
  )
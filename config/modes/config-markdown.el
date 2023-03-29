;; ========================================================================== ;;
;; CONFIG-MARKDOWN.EL --- configuration for markdown modes                    ;;
;; ========================================================================== ;;
;; This file is not part of GNU Emacs.

;; -------------------------------------------------------------------------- ;;
;; Global Settings                                                            ;;
;; -------------------------------------------------------------------------- ;;
(setq markdown-header-scaling t)        ; set before loading markdown-mode
(require 'markdown-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" .
               markdown-mode))

(add-to-list 'auto-mode-alist
             '("\\.md\\'" .
               gfm-mode))


;; ---------- GitHub Markdown Preview Window Settings ---------- ;;
(defvar gfm-preview-size '(720 650)
  "Size for the GFM Markdown preview window")

(defvar gfm-preview-position '(800 10)
  "Position for the GitHub Markdown preview window")

;; Style Sheet
(add-to-list 'markdown-css-paths
             (f-full "~/.emacs.d/etc/github-pandoc.css"))


;; -------------------------------------------------------------------------- ;;
;; Hooks                                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defun config-mode-markdown ()
  "Defaults for markdown mode."
  (auto-fill-mode 0)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-file)
  
  )

(defun config-mode-gfm ()
  "Defaults for GFM (GitHub Flavored Markdown) mode."
  (setq
   mode-name                    "GitHub Markdown"
   ;; tab-width                    2
   ;; fill-column                  99999999
   )
  
   )

(add-hook 'markdown-mode-hook               'config-mode-markdown)
(add-hook 'gfm-mode-hook                    'config-mode-gfm)


;; -------------------------------------------------------------------------- ;;
;; Frame Settings                                                             ;;
;; -------------------------------------------------------------------------- ;;


;; -------------------------------------------------------------------------- ;;
;; Keybinding                                                                 ;;
;; -------------------------------------------------------------------------- ;;
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do))

(define-keys        markdown-mode-map
  [(tab)]				#'complete-symbol
  
  ;; ---------- Styles ----------
  (kbd "C-i")           #'markdown-insert-italic
  (kbd "C-b")	        #'markdown-insert-bold 
  (kbd "C-k")			#'markdown-insert-link ; typical MS key combo 
  (kbd "C-j")			#'markdown-insert-code
  
  ;; ---------- Hide-Show ----------
  [(f3)]				#'hs-toggle-markdown
  [(shift f3)]			#'hs-toggle-all-markdown
  )

(define-keys        gfm-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]		#'gfm-preview
  )

  ;; (browse-url
  ;;  (concat "https://github.com/adam-p/markdown-here"
  ;; 		 "/wiki/Markdown-Cheatsheet"))


;; -------------------------------------------------------------------------- ;;
;; Syntax Highlighting                                                        ;;
;; -------------------------------------------------------------------------- ;;




;; -------------------------------------------------------------------------- ;;
;; Commands                                                                   ;;
;; -------------------------------------------------------------------------- ;;
(defun gfm-preview (&optional output-buffer-name)
  "Run `markdown-command' on the current buffer and view output in browser.

This is similar to `markdown-preview' with the following changes:
  - sets the `coding-system-for-write' to `utf-8-dos'
  - sets `markdown-command' to \"pandoc --from=gfm\"
  - previews using `browse-file-as-chrome-app' with arguments
    `gfm-preview-position', `gfm-preview-size'"
  (interactive)
  (let* ((coding-system-for-write 'utf-8-dos)
         (markdown-command "pandoc --from=gfm"))
    ;; Run `markdown' with the changed variables
    (setq output-buffer-name (markdown output-buffer-name))

    ;; Add the header and footer, set mode (no change from source)
    (with-current-buffer output-buffer-name
      (set-buffer output-buffer-name)
      (unless (markdown-output-standalone-p)
        (markdown-add-xhtml-header-and-footer output-buffer-name))
      (goto-char (point-min))
      (html-mode))

    ;; Browse buffer with `browse-file-as-chrome-app' 
    (cl-letf
        (((symbol-function 'browse-url-default-browser)
          (lambda (url &rest args)
            (browse-file-as-chrome-app
             url
             gfm-preview-position
             gfm-preview-size))))
      (browse-url-of-buffer output-buffer-name))))


;; ---------- Hide-Show ---------- ;;
;; TODO: Move to config-text.el
(defun hs-toggle-markdown ()
  "Show or hide the current heading depending on its current state."
  (interactive)
  (outline-toggle-children))

(defun hs-hide-all-markdown ()
  "Hide all levels in document except the highest."
  (interactive)
  (outline-hide-sublevels)
  ;; assuming the top level is the title, show this level
  ;; TODO: (unless (> (length HEAD_1) 1) (outline-toggle-children))
  (outline-toggle-children)
  (setq hs-hide-all-state t)
  )

(defun hs-show-all-markdown ()
  "Show all levels in document."
  (interactive)
  (outline-show-all)
  (setq hs-hide-all-state nil))

(defun hs-toggle-all-markdown ()
  "Toggles the document's folding.

This simply does the opposite of what previously done in the buffer."
  (interactive)
  (if hs-hide-all-state
	  (hs-show-all-markdown)
    (hs-hide-all-markdown)))


;; -------------------------------------------------------------------------- ;;
;; Functions                                                                  ;;
;; -------------------------------------------------------------------------- ;;


(provide 'config-markdown)
;;; CONFIG-MARKDOWN.EL ends here

;; ----------------------------------------------------------------------------
;; R Mode Setup
;; ----------------------------------------------------------------------------
(require 'config-programming)
(require 'repl)
;; (require 'ess-site)



(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:modifiers .   t)
	    (ess-R-fl-keyword:fun-defs .    t)
	    (ess-R-fl-keyword:keywords .    t)
	    (ess-R-fl-keyword:assign-ops .  t)
	    (ess-R-fl-keyword:constants .   t)
	    (ess-fl-keyword:fun-calls .     t)
	    (ess-fl-keyword:numbers .       t)
	    (ess-fl-keyword:operators       )
	    (ess-fl-keyword:delimiters      )
	    (ess-fl-keyword:= .             t)
	    (ess-R-fl-keyword:F&T .         t)
	    (ess-R-fl-keyword:%op% .        t))
      ess-ask-for-ess-directory         nil
      ess-eval-visibly                  nil
      ess-help-kill-bogus-buffers       t
      ess-help-own-frame                'one
      ess-history-file                  nil
      ess-keep-dump-files               nil
      ess-style                         'RStudio)

(setq-from-config :applications :r :exe)

(add-to-list 'hs-special-modes-alist
		     '(ess-mode "{" "}" "/[*/]" nil
				        hs-c-like-adjust-block-beginning))


;; -------------------------------------------------------------------------- ;;
;; Hooks                                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defun config-mode-r ()
  "Defaults for R programming (`ess-r-mode')."
  (repl-mode +1)

  (setq
   web-search-mode-prefix       "R"
   repl-interactive-mode        'inferior-ess-mode
   repl-function-eval   #'ess-eval-region-or-function-or-paragraph-and-step
   repl-function-eval-insert #'ess-eval-region-or-function-or-paragraph-and-step
   repl-function-set            #'R-set-repl
   repl-function-create         #'R-create-repl)

  )

(defun config-mode-r-interactive ()
  "Defaults for R REPL buffer (`inferior-ess-mode')."
  (repl-mode +1)
  )

(add-hook 'ess-r-mode-hook              'config-mode-r)
(add-hook 'inferior-ess-mode-hook       'config-mode-r-interactive)
;; (ess-help-mode			. (lambda () (font-lock-mode t))))) 


;; comint-move-point-for-output to 'others or t.

;; -------------------------------------------------------------------------- ;;
;; Keybinding                                                                 ;;
;; -------------------------------------------------------------------------- ;;
(with-eval-after-load "ess-site" 
  ;; ---------- Main Mode ---------- ;;
  (define-keys          ess-r-mode-map
    ;; [(return)]           'newline-and-indent
    (kbd "C-=")             #'ess-insert-assign

    ;; Indent / Tabs
    (kbd "<C-tab>")         'tab-to-tab-stop-magic
    (kbd "<tab>")           'indent-for-tab-command

    ;; Help
    "\C-p"			        'R-pander-style
    "\C-hf"      	        'R-object-help
    "\C-hv"      	        'R-object-str
    "\C-ho"      	        'R-object-summaries
    "\C-hn"      	        'R-object-names
    "\C-hV"      	        'ess-display-vignettes
    "\C-hH"      	        'ess-handy-commands
    )

  ;; ---------- Inferior Mode ---------- ;;
  (define-many-keys     inferior-ess-r-mode-map  
    (kbd "C-=")             #'ess-insert-assign    

    ;; Help
    "\C-hf"                 'R-object-help
    "\C-hv"      	        'R-object-str
    "\C-ho"      	        'R-object-summaries
    "\C-hn"      	        'R-object-names
    "\C-hV"      	        'ess-display-vignettes
    "\C-hH"      	        'ess-handy-commands) 

  ;; ---------- Help Mode ---------- ;;
  (define-key           ess-help-mode-map
    "q"                     'kill-buffer-or-emacs)
)


;; -------------------------------------------------------------------------- ;;
;; Commands                                                                   ;;
;; -------------------------------------------------------------------------- ;;

;; ---------- Help Commands ---------- ;;
(defun R-object-str ()
  "Get info for object at point"
  (interactive)
  (let ((objname (current-word)))    
    (ess-execute (concat "str(" objname ", max.level = 2)") t nil "INFO: ")
    (R-raise-frame-process)))

(defun R-object-names ()
  "Get names for object at point"
  (interactive)
  (let ((objname (current-word)))    
    (ess-execute (concat "names(" objname ")") t nil "INFO: ")
    (R-raise-frame-process)))

(defun R-object-summaries ()
  "Get summary for object at point"
  (interactive)
  (let ((objname (current-word t)))    
    (ess-execute (concat "cat('\n" objname
					     " ', format(object.size(" objname
					     "), units = 'MB', digits = 3), '\n')")
			     t nil "INFO: ")
    (R-raise-frame-process)))

(defun R-object-help (object &optional command)
  "This function makes several changes to `ess-display-help-on-object', however
  attempts to maintain the primary functionality.
    1) Removed ess-ddeclient-p / inferior-ess-help-filetype (not used
       personally)
    2) Changed the buffer name to simply '*Help [R]*'
    3) Removed `ess--switch-to-help-buffer'. Now uses simply `display-buffer'
       and additionally raises the frame.
    4) Uses `ess-find-help-file-auto' which will only prompt if the object is
       not found."
  
  (interactive
   (progn
     (ess-force-buffer-current)
     (when current-prefix-arg ;update cache if prefix 
       (with-current-buffer (process-buffer (ess-process-get
					                         ess-current-process-name))
	     (ess-process-put 'sp-for-help-changed? t)))
     (list (ess-find-help-file-auto "Help on"))))
  
  (let* ((hb-name	"*Help [R]*") 	; Simple Name
	     (old-hb-p	(get-buffer hb-name))
	     (tbuffer	(get-buffer-create hb-name)))
    (ess-with-current-buffer tbuffer
	  (setq ess-help-object object
	        ess-help-type 'help)
	  (ess--flush-help-into-current-buffer object command))
    (if old-hb-p			; Will raise the already existed frames
	    (save-frame-excursion
	     (show-a-frame-on "*Help [R]*"))
      (display-buffer "*Help [R]*")	; Display the buffer (the typical way)
      )))

(defun ess-find-help-file-auto (p-string)
  "Same as `ess-find-help-file', however if the object is
initially found it automatically shows the help without prompting."
  (ess-make-buffer-current)
  (if ess-get-help-topics-function
      (let* ((help-files-list (funcall
			                   ess-get-help-topics-function
			                   ess-current-process-name))
	         (hlpobjs (ess-helpobjs-at-point
		               help-files-list)))
	    (if (car hlpobjs)
	        (car hlpobjs)
	      (ess-completing-read p-string (append (delq nil hlpobjs)
						                        help-files-list)
			                   nil nil nil nil (car hlpobjs))))
    ;; (string-match "\\(XLS\\)\\|\\(STA\\)\\|\\(SAS\\)" ess-language)
    (read-string (format "%s: " p-string))))

(defun R-object-sizes ()
  "Get object sizes over 500kb"
  (interactive)
  (ess-execute "data.frame(Mb = round(sapply(ls(),
                             function(x) {
                               object.size(get(x))/1024^2
                             }), 3)) %>%
  rownames_to_column('object') %>%
  filter(Mb > 0.5) %>%
  arrange(-Mb)" t nil "INFO: ")
  (R-raise-frame-process))



;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting
;; ------------------------------------------------------------------------- ;;
;; (font-lock-add-keywords
;;  ;; font-lock-remove-keywords
;;  'ess-mode
;;  '(("\\(\\sw\\|\\s\"\\|\\s-\\)\
;; \\([!<=>]=\\|[!<>]\\|[&|]\\{1,2\\}\\)\
;; \\(\\sw\\|\\s\"\\|\\s-\\|$\\)"
;;     2
;;     'font-lock-relation-operator-face))
;;  '(("\\(\\s-\\|^\\|\\s\(\\)\\(!\\)"
;;     2
;;     'font-lock-relation-operator-face)))

(provide 'config-r)
;;; CONFIG-R.EL ends here

;; ----------------------------------------------------------------------------
;; R Mode Setup
;; ----------------------------------------------------------------------------
(provide 'r-setup)
;; (require 'ess-site)

(setq-default
 ess-local-process-name		"R"			; Set Process Name
 ess-S-assign-key			(kbd "C-=")	; StatET-like assignment 
)

(ess-toggle-S-assign-key		t)	     ; enable above key definition
(ess-toggle-underscore		nil)	     ; leave my underscore alone
(add-to-list 'hs-special-modes-alist
		   '(ess-mode "{" "}" "/[*/]" nil
				    hs-c-like-adjust-block-beginning))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun my-r-mode-hook ()  
  (interactive)  

  (hs-minor-mode t)
  (hs-hide-all)
  (setq ac-sources
	   (append '(ac-source-R-objects
			   ac-source-R-args
			   ac-source-yasnippet) ac-sources))
  (flyspell-prog-mode)
  (turn-on-auto-fill)  
  (rainbow-delimiters-mode 1)
  )

(defun my-inferior-r-mode-hook ()
  (text-scale-set -1.1)

  (setq ac-sources
	   (append '(ac-source-R-objects
			   ac-source-R-args
			   ac-source-yasnippet) ac-sources))
  )

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys ess-mode-map
  ;; [(return)]		'newline-and-indent

  ;; ---------- Evaluation ----------
  [(shift return)]     'R-eval
  [(M-return)]		   'R-pander-insert
  ;; 'ess-use-this-dir

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'indent-for-tab-command
  
  
  ;; ---------- Help ----------
  [(f1)]		   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "R "))

  "\C-p"			'R-pander-style
  "\C-hf"      	'R-object-help
  "\C-hv"      	'R-object-str
  "\C-ho"      	'R-object-summaries
  "\C-hn"      	'R-object-names
  "\C-hV"      	'ess-display-vignettes
  "\C-hH"      	'ess-handy-commands

  ;; ---------- Frame Switching ----------
  [(f12)]			'R-switch-frame-process
  [S-f12]			'R-process-new
  [C-f12]			'ess-switch-process
  )

(define-many-keys inferior-ess-mode-map
  ;; ---------- Input / Prompt Scrolling ----------
  [C-up]               'comint-previous-prompt
  [C-down]             'comint-next-prompt
  [up]                 'comint-previous-input
  [down]               'comint-next-input
  [S-C-up]			'previous-line
  [S-C-down]			'next-line

  
  ;; ---------- Completion ----------
  ;; (kbd "<tab>")	'completion-at-point


  ;; ---------- Help ----------
  [(f1)]		   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t "R "))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil "R "))

  "\C-hf"      	'R-object-help
  "\C-hv"      	'R-object-str
  "\C-ho"      	'R-object-summaries
  "\C-hn"      	'R-object-names
  "\C-hV"      	'ess-display-vignettes
  "\C-hH"      	'ess-handy-commands
  
  ;; ---------- Frame Switching ----------
  [(f12)]			'R-switch-frame-script
  [S-f12]			'R-process-new
  ) 

(define-key ess-help-mode-map	"q" 'kill-buffer-or-emacs)

;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------

;; ---------- Evaluation ---------- ;;
(defun R-eval ()
  "Evaluates R code."
  (interactive)
  ;; Pre Eval
  (when (eq (length (switch-frame-buffer-list '("\\*R.*") '("^ "))) 0)
    (R-kill-all-processes)              ; No Buffers so kill them anyways
    (R-process-new))
  
  ;; Eval
  (if (and transient-mark-mode mark-active)
      (R-eval-region)
    (R-eval-paragraph)))

(defun R-eval-region ()
  "Evaluates R region and returns back to current frame."
  (interactive)
  (call-interactively 'ess-eval-region)
  (R-raise-frame-process)
  (deactivate-mark))

(defun R-eval-paragraph ()
  "Evaluates R region and returns back to current frame."
  (interactive)
  (call-interactively 'ess-eval-function-or-paragraph-and-step)
  (R-raise-frame-process))


;; ---------- Process Commands ---------- ;;
(defun R-process-new ()
  "Creates a new R-process."
  (interactive)
  (R)
  (R-switch-frame-script))

(defun R-kill-all-processes ()
  "Kills all R processes and clears the name-list."
  (interactive)
  (mapcar '(lambda (arg)
             (when (get-process (car arg))
               (kill-process (get-process (car arg)))))
          ess-process-name-list)
  
  (mapcar 'kill-buffer (switch-frame-buffer-list '("\\*R.*") '("^ ")))
  (setq ess-process-name-list nil))


;; ---------- Frame Commands ---------- ;;
(defun R-switch-frame-process ()
  "Switch to associated process, associate with one, or create one."
  (interactive)
  ;; Does current buffer have an associated process?
  ;; Yes -> raise and select
  ;; No -> are there processes running?
  ;; Yes -> associate -> raise
  ;; No -> create one -> associate -> raise 
  (ess-switch-to-ESS t))

(defun R-raise-frame-process ()
  (save-frame-excursion
   (raise-frame
    (get-frame (ess-get-process-buffer)))))

(defun R-switch-frame-script ()
  "Switch to most recent script buffer."
  (interactive)
  (let ((dialect ess-dialect)
	   (loc-proc-name ess-local-process-name)
	   (blist (cdr (buffer-list))))
    (while (and blist
			 (with-current-buffer (car blist)
			   (not (or (and
					   (memq major-mode '(ess-mode ess-julia-mode))
					   (equal dialect ess-dialect)
					   (null ess-local-process-name))
					  (and 
					   (memq major-mode '(ess-mode ess-julia-mode))
					   (equal loc-proc-name ess-local-process-name))
					  ))))
	 (pop blist))
    (if blist
	   (ess-show-buffer (car blist) t)
	 (message "Found no buffers for ess-dialect %s associated with process %s"
			dialect loc-proc-name))))


;; ---------- Echo Results / Pander ---------- ;;
(defun R-pander-wrap (string)
  (format "pander:::panderOptions('table.alignment.default', 'right')\n
pander:::pander({%s\n}, style = \"%s\")\n"
		string "simple"))

(defun R-eval-pander ()
  "Evaluates R code and formats output using Pander. Customize the style using `R-pander-style'"
  (interactive)
  (add-hook 'ess-presend-filter-functions 'R-pander-wrap)
  (R-eval)
  (remove-hook 'ess-presend-filter-functions 'R-pander-wrap)
  )

(defun R-pander-insert ()
  (interactive)
  (let
	 ((pt-beg 0)
	  (pt-end 0)
	  (comment-str (concat comment-start comment-start " ")))
    
    ;; Get the begin and end point of code
    (save-excursion
	 (unless (region-active-p)
	   (ess-mark-function-or-para)
	   (setq pt-end -1))
	 (setq pt-beg (region-beginning))
	 (setq pt-end (+ pt-end (region-end))))
    
    (goto-char pt-end)
    (newline)

    (ess-command (R-pander-wrap (buffer-substring pt-beg pt-end))
			  nil nil nil nil (get-process "R"))
    
    (save-excursion
    	 (set-buffer (get-buffer-create " *ess-command-output*"))	 

	 ;; Trim empty lines at the end (usually 2)
	 (goto-char (point-max))
    	 (beginning-of-line)
    	 (cl-loop
	  while (and (not (bobp))
			   (looking-at "^[[:space:]]*$"))
	  do (forward-line -1)
	  )
	 (forward-line)
    	 (kill-region (1- (point)) (point-max))

    	 ;; Trim junk lines at the start
	 (goto-char (point-min)) 
	 (kill-line)
    	 (cl-loop
    	  while (and (not (eobp))
			   (looking-at "^[[:space:]]*$"))
    	  do (kill-line))
	 
    	 ;; Insert comments on line
    	 ;; (insert comment-str)
    	 (cl-loop
	  until (eobp)
	  do
	  (insert comment-str)
	  (forward-line 1))
	 )
    (insert-buffer-substring " *ess-command-output*"))
  (ess-next-code-line 1))

(defun R-pander-style (style)
  "Sets the Pander style for `R-eval-pander'"
  (interactive (list (completing-read
				  "Style: "
				  '("multiline" "grid" "simple"
				    "rmarkdown" "jira"))))
  (fset 'R-pander-wrap
	   `(lambda (string)
		 (format
		  "pander:::panderOptions('table.alignment.default', 'right')\n
pander:::pander({%s}, style = \"%s\")\n"
		  string ,style)))
  )


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
(
 font-lock-add-keywords
 ;; font-lock-remove-keywords
 'ess-mode
 '(("\\(\\sw\\|\\s\"\\|\\s-\\)\
\\([!<=>]=\\|[!<>]\\|[&|]\\{1,2\\}\\)\
\\(\\sw\\|\\s\"\\|\\s-\\|$\\)"
    2
    'font-lock-relation-operator-face))
 '(("\\(\\s-\\|^\\|\\s\(\\)\\(!\\)"
    2
    'font-lock-relation-operator-face)))

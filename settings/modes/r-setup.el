;; ----------------------------------------------------------------------------
;; R Mode Setup
;; ----------------------------------------------------------------------------
(provide 'r-setup)
(require 'ess-site)

(setq-default
 inferior-R-args			"--no-restore-history --no-save"
 ess-ask-for-ess-directory     nil			; Suppress ask for directory
 ess-local-process-name		"R"			; Set Process Name
 ess-r-args-show-as			'tooltip		; R ARGS as tool tip
 ess-r-args-show-prefix		""			; Remove ARG Prefix
 ess-r-args-noargsmsg		"No Args"
 ess-help-kill-bogus-buffers	t			; Kill silly buffers
 ess-eval-visibly-p			nil
 ess-help-own-frame			1
 ess-S-assign-key			(kbd "C-=")	; StatET-like assignment
 ;;inferior-R-program-name "C:\\Program Files\\R\\R-2.5.0\\bin\\Rterm.exe"
 ess-history-file			nil
 ess-default-style			'RStudio
 ;; inferior-ess-r-help-command	".ess.help('%s', 'text')"
)

(ess-toggle-S-assign-key		t)	     ; enable above key definition
(ess-toggle-underscore		nil)	     ; leave my underscore alone


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'ess-mode-hook		'my-r-mode-hook)

(defun my-r-mode-hook ()  
  (interactive)  

  (hs-minor-mode t)
  (add-to-list 'hs-special-modes-alist
	       '(ess-mode "{" "}" "/[*/]" nil
			  hs-c-like-adjust-block-beginning))
  (hs-hide-all)
  (turn-on-auto-fill)

  (add-to-list 'ac-sources 'ac-source-R-objects)
  (add-to-list 'ac-sources 'ac-source-R-args)
  
  (flyspell-prog-mode)
  )

(add-hook 'inferior-ess-mode-hook	'my-inferior-r-mode-hook)
(defun my-inferior-r-mode-hook ()
  (text-scale-set -1.1)

  (add-to-list 'ac-sources 'ac-source-R-objects)
  (add-to-list 'ac-sources 'ac-source-R-args)
  )

(add-hook
 'ess-help-mode-hook
 '(lambda () (font-lock-mode t)))

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys ess-mode-map
  ;; [(return)]		'newline-and-indent

  ;; ---------- Evaluation ----------
  [(shift return)]     'R-eval
  

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'indent-for-tab-command
  
  
  ;; ---------- Help ----------
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
  [(f12)]              'switch-frame-current-R
  [S-f12]              'R-process-new
  [C-f12]              'ess-switch-process
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
  [(f12)]          'switch-frame-previous
  [S-f12]		'R-process-new
  )

(define-key ess-help-mode-map	"q" 'kill-buffer-or-emacs)

;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
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
  (save-frame-excursion
   (call-interactively 'ess-eval-region)
   (switch-frame-current-R))
  (deactivate-mark))

(defun R-eval-paragraph ()
  "Evaluates R region and returns back to current frame."
  (interactive)
  (save-frame-excursion
   (call-interactively 'ess-eval-function-or-paragraph-and-step)
   (switch-frame-current-R)))

(defun R-process-new ()
  "Creates a new R-process."
  (interactive)
  (save-window-excursion (R))
  (save-frame-excursion
   (switch-frame-previous-R)))

(defun switch-frame-next-R ()
  "Cycle through the R buffers."
  (interactive)
  (switch-frame-next-buffer '("\\*R.*") '("^ ")))

(defun switch-frame-current-R ()
  "Switch to current R process buffer."
  (interactive)
  (let (b)
  (if (>= (length (switch-frame-buffer-list '("\\*R.*") '("^ "))) 1)
      (progn
	(setq b (process-buffer (get-process
				 ess-current-process-name)))
	(raise-frame (get-frame b))
	(set-buffer b)
	(end-of-buffer-all))
    (R-process-new))))

(defun switch-frame-previous-R ()
  "Switch to previous R buffer."
  (interactive)
  (switch-frame-previous-buffer '("\\*R") '("^ ")))

(defun R-kill-all-processes ()
  "Kills all R processes and clears the name-list."
  (interactive)
  (mapcar '(lambda (arg)
             (when (get-process (car arg))
               (kill-process (get-process (car arg)))))
          ess-process-name-list)
  
  (mapcar 'kill-buffer (switch-frame-buffer-list '("\\*R.*") '("^ ")))
  (setq ess-process-name-list nil))


;; -----------------------------------------------------------------------------
;;  Help Functions
;; -----------------------------------------------------------------------------
(defun R-object-str ()
  "Get info for object at point"
  (interactive)
  (let ((objname (current-word)))
    (ess-execute (concat "str(" objname ", max.level = 1)") t nil "INFO: ")
    (switch-frame-current-R)))

(defun R-object-names ()
  "Get names for object at point"
  (interactive)
  (let ((objname (current-word)))
    (ess-execute (concat "names(" objname ")") t nil "INFO: ")
    (switch-frame-current-R)))

(defun R-object-summaries ()
  "Get summary for object at point"
  (interactive)
  (let ((objname (current-word t)))
    (ess-execute (concat "cat('\n" objname
					" ', format(object.size(" objname
					"), units = 'MB', digits = 3), '\n')")
			  t nil "INFO: ")
    (switch-frame-current-R)))

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
  )




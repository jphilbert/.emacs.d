;; ----------------------------------------------------------------------------
;; R Mode Setup
;; ----------------------------------------------------------------------------
(provide 'r-setup)
(require 'ess-site)

(setq
 inferior-R-args		"--no-restore-history --no-save"
 ess-ask-for-ess-directory      nil          ; Suppress ask for directory
 ess-local-process-name		"R"          ; Set Process Name
 ess-r-args-show-as		'tooltip     ; R ARGS as tool tip
 ess-r-args-show-prefix		""           ; Remove ARG Prefix
 ess-r-args-noargsmsg		"No Args"
 ess-help-kill-bogus-buffers	t            ; Kill silly buffers
 ess-eval-visibly-p		nil
 ess-help-own-frame		1
 ess-S-assign-key		(kbd "C-=")) ; StatET-like assignment

(ess-toggle-S-assign-key	t)	     ; enable above key definition
(ess-toggle-underscore		nil)	     ; leave my underscore alone

;; --------------------------------------------------------------------------
;; Multi-Window
;; --------------------------------------------------------------------------
(add-to-list
 'special-display-regexps
 (list ".*\\*R.*\\*.*" 'display-default-frame
       (list '(unsplittable . nil)
             '(horizontal-scroll-bars . nil)
             '(vertical-scroll-bars . nil)
             '(height . 50)             ; Appears to autofit
             '(width . 80)
             '(top . 10)
             (cons 'left (+ (/ (x-display-pixel-width) 2) 20)))))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'ess-mode-hook		'my-r-mode-hook)
(defun my-r-mode-hook ()
  (require 'ac-R)
  (add-to-list 'ac-modes 'ess-mode)
  ;; (add-to-list 'ac-sources 'ac-source-R)
  
  (hs-minor-mode t)
  (setq ac-auto-show-menu t)
  (auto-indent-minor-mode 1)
  (add-to-list 'hs-special-modes-alist
	       '(ess-mode "{" "}" "/[*/]" nil
			  hs-c-like-adjust-block-beginning))
  
  (hs-hide-all)
  (flyspell-prog-mode)
  (ac-flyspell-workaround)
  
  ;; --------------------------------------------------------------------------
  ;; Keybinding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   (kbd "<tab>")	'ess-complete-object-name
   [(return)]		'newline
   [(shift return)]     'R-eval
   
   [(f12)]              'switch-frame-current-R
   [S-f12]              'R-process-new
   [C-f12]              'ess-switch-process
   
   (kbd "(")            'ess-r-args-auto-show
   (kbd "[")    	'skeleton-pair-insert-maybe
   (kbd "{")    	'skeleton-pair-insert-maybe
   (kbd "\"")   	'skeleton-pair-insert-maybe
   (kbd "\'")   	'skeleton-pair-insert-maybe
   (kbd "\`")   	'skeleton-pair-insert-maybe
   (kbd "%")    	'skeleton-pair-insert-maybe
   
   "\C-hf"      	'ess-display-help-on-object
   ;; "\C-hw"   	'r-help-web
   "\C-hv"      	'R-object-str
   "\C-ho"      	'R-object-summaries
   "\C-hn"      	'R-object-names))

(add-hook 'inferior-ess-mode-hook	'my-inferior-r-mode-hook)
(defun my-inferior-r-mode-hook ()
  (auto-complete-mode)

  (text-scale-set -1.1)
  
  (setq
   ;; terminal-buffer		"*R*"
   ess-history-file		"/dev/null")
  
  ;; --------------------------------------------------------------------------
  ;; Key-binding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   (kbd "<tab>")        'ess-complete-object-name
   [C-up]               'comint-previous-prompt
   [C-down]             'comint-next-prompt
   [up]                 'comint-previous-input
   [down]               'comint-next-input
   
   [(f12)]              'switch-frame-next-R
   [S-f12]		'R-process-new
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe
   (kbd "%")            'skeleton-pair-insert-maybe
   
   "\C-hf"              'ess-display-help-on-object
   ;; "\C-hw"           'r-help-web
   "\C-hv"              'R-object-str
   "\C-ho"              'R-object-summaries
   "\C-hn"      	'R-object-names))

;; Aesthetics
(add-hook
 'ess-mode-hook
 '(lambda ()
    (font-lock-add-keywords
     nil
     '(("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*("
        1                               ; Signifies which group
        'font-lock-ess-functions-face)
       ("\\s\"?\\(\\(\\sw\\|\\s_\\)+\\(<-\\)?\\)\\s\"?*\\s-*[[\\|$]"
        1                               ; Signifies which group
        'font-lock-ess-dataframe-face)
       ("[0-9]+"                        ; Put near end
        .
        'font-lock-number-face)
       ("\\(?: \\(?:\\(?:[!<=>]=\\|[<>]\\) \\)\\)"
        .
        'font-lock-relation-operator-face)))))
(add-hook
 'ess-help-mode-hook
 '(lambda ()
    (font-lock-mode t)
    ;; (setq font-lock-keywords t)
    (font-lock-add-keywords
     nil '(("\\(?:Description\\|Usage\\|Arguments\\|Details\\|Value\\|S4 methods\\|References\\|See Also\\|Examples\\):"
            .
            'font-lock-ess-help-heading-2-face)))))

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
   (process-send-string
    (get-ess-process ess-current-process-name)
    "cat(\"\\n\") \n")
   (switch-frame-current-R))
  (deactivate-mark))

(defun R-eval-paragraph ()
  "Evaluates R region and returns back to current frame."
  (interactive)
  (save-frame-excursion
   (call-interactively 'ess-eval-function-or-paragraph-and-step)
   (process-send-string
    (get-ess-process ess-current-process-name)
    "cat(\"\\n\") \n")
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
  (if (>= (length (switch-frame-buffer-list '("\\*R.*") '("^ "))) 1)
      (raise-frame
       (get-frame
	(process-buffer (get-process
			 ess-current-process-name))))
    (R-process-new)))

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
    (ess-execute (concat "object.summaries(\"" objname "\")") t nil "INFO: ")
    (switch-frame-current-R)))


;; -----------------------------------------------------------------------------
;;  Redefined Functions
;; -----------------------------------------------------------------------------
(require 'ess-help) 			; Might need to load this prior to
					;  overload 

;; Changed for better popup utilization
(defun ess-display-help-on-object (object) 
  "sdasd"
  (interactive
   (if (ess-ddeclient-p)
       (list (read-string "Help on: "))
     (ess-find-help-file "Help on: ")))

  (if (or (ess-ddeclient-p)
          (equal inferior-ess-help-filetype "chm"))
      (if (ess-ddeclient-p)
          (ess-display-help-on-object-ddeclient object) ;; ddeclient version
        (ess-eval-linewise (concat "help(" object ")"))) ;; "chm" version

    ;; else: "normal", non-DDE behavior:
    (let* ((hb-name (concat "*Help["
                            ess-current-process-name
                            "]*"))      ; JPH Change
           (old-hb-p   nil)
           (curr-win-mode major-mode)
           (tbuffer     (get-buffer-create hb-name))
           (curr-help-command           inferior-ess-help-command)
           ;;-- pass the buffer-local 'ess-help-sec-..'  to the ess-help buffer:
           (curr-help-sec-regex         ess-help-sec-regex)
           (curr-help-sec-keys-alist    ess-help-sec-keys-alist)
           (curr-help-syntax-table      (syntax-table))
           (curr-help-filetype          inferior-ess-help-filetype)
           (alist               ess-local-customize-alist))

      (set-buffer tbuffer)
      (ess-setq-vars-local (eval alist))
      (setq ess-help-sec-regex    curr-help-sec-regex)
      (setq ess-help-sec-keys-alist curr-help-sec-keys-alist)
      (setq inferior-ess-help-filetype curr-help-filetype)
      ;; see above, do same for inferior-ess-help-command... (i.e. remove
      ;; hack, restore old code :-).
      
      ;; JPH -- DO THIS ALWAYS
      ;; (if (or
      ;;           current-prefix-arg
      ;;           ;; (ess-help-bogus-buffer-p old-hb-p nil nil 'debug)
      ;;         )


      ;; Ask the corresponding ESS process for the help file:
      ;; (progn
      (if buffer-read-only (setq buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (ess-help-mode)
      (setq ess-local-process-name ess-current-process-name)
      (ess-command (format curr-help-command object) tbuffer)
      ;; was inferior-ess-help-command

      (ess-help-underline)
      ;; Stata is clean, so we get a big BARF from this.
      (if (not (string= ess-language "STA"))
          (ess-nuke-help-bs))

      (goto-char (point-min))

      (save-excursion
        (let ((PM (point-min))
              (nodocs
               (ess-help-bogus-buffer-p (current-buffer) nil 'give-match )))
          (goto-char PM)
          (if (and nodocs
                   ess-help-kill-bogus-buffers)
              (progn
                (if (not (listp nodocs))
                    (setq nodocs (list PM (point-max))))
                (ess-write-to-dribble-buffer
                 (format "(ess-help: error-buffer '%s' nodocs (%d %d)\n"
                         (buffer-name) (car nodocs) (cadr nodocs)))
                ;; Avoid using 'message here -- may be %'s in string
                ;;(princ (buffer-substring (car nodocs) (cadr nodocs)) t)
                ;; MM [3/2000]: why avoid?  Yes, I *do* want message:
                (message "%s" (buffer-substring (car nodocs) (cadr nodocs)))
                ;; ^^^ fixme : remove new lines from the above {and abbrev.}
                (ding)
                (kill-buffer tbuffer))

            ;; else : show the help buffer.

            ;; Check if this buffer describes where help can be found in
            ;; various packages. (R only).  This is a kind of bogus help
            ;; buffer, but it should not be killed immediately even if
            ;; ess-help-kill-bogus-buffers is t.

            ;; e.g. if within R, the user does:

            ;; > options("help.try.all.packages" = TRUE)
	    
            ;; > ?rlm
	    
            ;; then a list of packages for where ?rlm is defined is
            ;; shown.  (In this case, rlm is in package MASS).  This
            ;; help buffer is then renamed *help[R](rlm in packages)* so
            ;; that after MASS is loaded, ?rlm will then show
            ;; *help[R](rlm)*

            (if (equal inferior-ess-program inferior-R-program-name)
                ;; this code should be used only for R processes.
                (save-excursion
                  (goto-char (point-min))
                  (if (looking-at "Help for topic")
                      (let
                          ( (newbuf
                             (concat "*help[" ess-current-process-name
                                     "](" object " in packages)*")))
                        ;; if NEWBUF already exists, remove it.
                        (if (get-buffer newbuf)
                            (kill-buffer newbuf))
                        (rename-buffer  newbuf)))))

            (display-buffer tbuffer) ; jph Added
            (if curr-help-syntax-table
                (set-syntax-table curr-help-syntax-table))
	    (set-buffer-modified-p 'nil)
	    (toggle-read-only t)))))))

;; Added skeletion-pairing
(defun ess-r-args-auto-show ()
  "Typically assigned to \"(\": If there's an ess-process, automatically show arguments
and their default values of an R function. Built on \\[ess-r-args-show]."
  (interactive)
  ;; (insert "(");
  (skeleton-pair-insert-maybe nil)
  (if (and ess-local-process-name ; has a process and it must still be running
	   (get-ess-process ess-local-process-name))
      (ess-r-args-show)))


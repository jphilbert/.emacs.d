;; ----------------------------------------------------------------------------
;; My Python Setup
;; ----------------------------------------------------------------------------
(provide 'python-setup)

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
(require 'python-mode)

(require 'ipython)


(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


(add-to-list 'py-shell-alist, '("ipython" quote ipython))

;; --------------------------------------------------------------------------
;; Multi-Window
;; --------------------------------------------------------------------------
;; (add-to-list
;;  'special-display-regexps
;;  (list ".*\\*Python.*\\*.*" 'display-default-frame
;;        (list '(unsplittable . nil)
;;              '(horizontal-scroll-bars . nil)
;;              '(vertical-scroll-bars . nil)
;;              '(height . 50)
;;              '(width . 80)
;;              '(top . 40)                ; Offsets the Terminals
;;              (cons 'left (+ (/ (x-display-pixel-width) 2) 60)))))
;; (add-to-list
;;  'special-display-regexps
;;  (list ".*\\*.*rope-pydoc*\\*.*" 'display-*rope-pydoc*-frame
;;        (list '(horizontal-scroll-bars . nil)
;;              '(vertical-scroll-bars . nil)
;;              '(height . 40)
;;              '(top . 10)
;;              (cons 'left (/ (x-display-pixel-width) 2))
;;              ;; '(minibuffer . nil)
;;              )))
;; (defun display-*rope-pydoc*-frame (buf &optional args)
;;   "Display *rope-pydoc* buffer in its own frame.
;; `special-display-function' is used to do the actual displaying.
;; BUF and ARGS are the arguments to `special-display-function'."
;;   (let ((calling-frame (selected-frame))
;;         (return-window (select-window
;;                         (funcall special-display-function buf args))))
;;     (raise-frame)
;;     (fit-frame)                         ; Fit the frame to its context
;;     (setq mode-line-format nil)         ; Remove ModeLine (note do this after
;;     (raise-frame calling-frame)
;;     return-window))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'python-mode-hook             'my-python-mode-hook)
(defun my-python-mode-hook ()
  (auto-complete-mode 1)
  
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
  (set (make-local-variable 'ac-find-function) 'ac-python-find)
  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
  (set (make-local-variable 'ac-auto-start) nil)
  
  (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
  (yas/initialize)
  
  (rope-open-project "C:/python27/")

  (setq ac-auto-show-menu t)
  (define-key ac-completing-map (kbd "<tab>") 'ac-expand)
  (define-key ac-completing-map [(return)] 'ac-complete)
  
  (auto-indent-minor-mode -1)
  
  (custom-set-variables
   '(py-shell-switch-buffers-on-execute nil))
  
  ;; (add-to-list 'ac-modes 'python-mode)
  
  ;; (eldoc-mode 1)
  
  (set-variable 'py-indent-offset 4)
  (set-variable 'py-smart-indentation t)
  (set-variable 'indent-tabs-mode nil)
  
  ;;(highlight-beyond-fill-column)
  ;; (define-key python-mode-map "\C-m" 'newline-and-indent)
  ;; (pabbrev-mode)
  ;; (abbrev-mode)
  
  (flyspell-prog-mode)
  
  ;; --------------------------------------------------------------------------
  ;; Keybinding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   (kbd "<tab>")        'ryan-python-tab
   [(return)]		'newline
   [(shift return)]     'python-eval
   
   [(f12)]              'switch-frame-current-python
   ;; [S-f12]              'R-process-new
   ;; [C-f12]              'ess-switch-process
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe
   (kbd "%")            'skeleton-pair-insert-maybe
   
   "\C-hf"              'rope-show-doc
   ;; ;; "\C-hw"           'r-help-web
   ;; "\C-hv"              'R-object-str
   ;; "\C-ho"              'R-object-summaries
   ;; "\C-hn"      	'R-object-names
   (kbd "C-<")		'py-shift-region-left
   (kbd "C->")		'py-shift-region-right))

(add-hook 'py-shell-hook		'my-python-shell-hook)
(defun my-python-shell-hook ()
  (auto-complete-mode)
  (setq font-lock-keywords      python-font-lock-keywords)
  
  ;; --------------------------------------------------------------------------
  ;; Key-binding
  ;; --------------------------------------------------------------------------
  (local-set-many-keys
   ;; (kbd "<tab>")        'ess-complete-object-name
   ;; [C-up]               'comint-previous-prompt
   ;; [C-down]             'comint-next-prompt
   ;; [up]                 'comint-previous-input
   ;; [down]               'comint-next-input
   
   [(f12)]              'switch-frame-next-python
   ;;[S-f12]              'R-process-new
   
   (kbd "(")            'skeleton-pair-insert-maybe
   (kbd "[")            'skeleton-pair-insert-maybe
   (kbd "{")            'skeleton-pair-insert-maybe
   (kbd "\"")           'skeleton-pair-insert-maybe
   (kbd "\'")           'skeleton-pair-insert-maybe
   (kbd "\`")           'skeleton-pair-insert-maybe
   (kbd "%")            'skeleton-pair-insert-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          
;;; Auto-completion                      
;;;  Integrates:                                    
;;;   1) Rope
;;;   2) Yasnippet        
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")
(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))
(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

;;Ryan's python specific tab completion                                                                        
(defun ryan-python-tab ()
  ;; Try the following:                   
  ;; 1) Do a yasnippet expansion         
  ;; 2) Do a Rope code completion         
  ;; 3) Do an indent                  
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))


;; --------------------------------------------------------------------------
;; Helpers
;; --------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; Evaluation Methods
;; --------------------------------------------------------------------------
(defun python-eval ()
  "Evaluates python code."
  (interactive)
  ;; Pre Eval
  (when (eq (length (switch-frame-buffer-list '("\\*python.*") '("^ "))) 0)
    (python-process-new))
  
  ;; Eval
  (if (and transient-mark-mode mark-active)
      (python-eval-region)
    (python-eval-paragraph)))

(defun python-eval-line (&optional step)
  "Evaluates the current line to the python."
  (interactive ())
  (let ((com (buffer-substring (point-at-bol) (point-at-eol))))
    (if (> (length com) 0)
        (progn
          (python-eval-region (get-python-buffer) com)
          (when step (py-next-statement)))
      (message "No command in this line"))))

(defun python-eval-line-and-step ()
  "Evaluates the current line to the python and goes to next line."
  (interactive)
  (python-eval-line t))

(defun python-eval-region (start end)
  "Sends a region to the python."
  (interactive "r")
  (py-execute-region))

(defun python-eval-buffer ()
  "Evaluate whole buffer to the python."
  (interactive)
  (py-execute-buffer))

(defun python-eval-paragraph ()
  "Sends paragraph to python"
  (interactive)
  (save-frame-excursion
   ;; (beginning-of-line)
   (ignore-errors  
     (py-beginning-of-block))
   (backward-paragraph)
   (forward-line)
   (py-mark-block)
   (call-interactively 'py-execute-region))
  (py-next-statement 1))


;; --------------------------------------------------------------------------
;; Process Window Control
;; --------------------------------------------------------------------------
(defun python-process-new ()
  "Creates a new python-process."
  (interactive)
  (py-shell)
  (switch-frame-previous))

(defun switch-frame-next-python ()
  "Cycle through the python buffers."
  (interactive)
  (switch-frame-next-buffer '("\\*python.*") '("^ ")))

(defun switch-frame-current-python ()
  "Switch to current python process buffer."
  (interactive)
  (if (>= (length (switch-frame-buffer-list '("\\*python.*") '("^ "))) 1)
      (switch-frame-previous-buffer '("\\*python") '("^ "))
    (python-process-new)))

(defun switch-frame-previous-python ()
  "Switch to previous python buffer."
  (interactive)
  (switch-frame-previous-buffer '("\\*python") '("^ ")))
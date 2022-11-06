;; ----------------------------------------------------------------------------
;; SQL MODE
;; ----------------------------------------------------------------------------
(require 'sql)
(require 's)
(require 'f)

;; https://github.com/alex-hhh/emacs-sql-indent
;; (require 'sql-indent)             ; auto-loaded

(defun config-parse-sql-connection (con)
  (-let*
      (((con (&plist :product prod
                     :server srv
                     :database db
                     :uid uid
                     :pw pwd)) con)
       (con (s-chop-prefix ":" (symbol-name con)))
       (uid (or uid ""))
       (pwd (or pwd ""))
       (con (list con
                  `(sql-product     ',(intern prod))
                  `(sql-user        ,uid)
                  `(sql-password    ,pwd))))
    (when srv
      (add-to-list 'con  `(sql-server    ,srv) t))
    (when db
      (add-to-list 'con  `(sql-database  ,db) t))
    con
    ))
(defun config-parse-all-sql-connections ()
  (-map #'config-parse-sql-connection
        (-partition 2
                    (config-get :applications :sql :servers))))

(make-variable-buffer-local  'sql-product)
(setq
 sql-connection-alist       (config-parse-all-sql-connections)
 sql-product                'oracle
 sql-send-terminator        nil

 sql-ms-options             '("-w" "2000" "-y" "2000" "-s" "|" "-k")
 sql-ms-program             (or (config-get :applications :sql :exe :ms)
                                sql-ms-program)
 
 sql-oracle-scan-on         nil 
 sql-oracle-program         (or (config-get :applications :sql :exe :oracle)
                                sql-oracle-program))

;; TODO advise sql-set-product
;; -> google-mode-prefix (format "SQL %s " sql-product)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun config-mode-sql ()
  (interactive)
  (make-local-variable  'sql-product)
  (sql-set-product      'oracle)
  (sqlind-minor-mode +1)
  (sqlind-set-indentation)
  (sqlup-mode +1)

  (repl-mode +1)
  
  (setq
   mode-name                    "SQL"
   repl-interactive-mode        'sql-interactive-mode
   repl-function-eval           #'sql-eval
   repl-function-eval-insert    #'sql-eval-insert
   repl-function-select         #'sql-select-repl
   repl-function-create         #'sql-connect)

  (setq-local completion-at-point-functions
		'(cape-dabbrev cape-file)
		cape-dabbrev-min-length 4
        dabbrev-case-distinction
        dabbrev-check-all-buffers)
  )

(defun config-mode-sql-interactive ()
  (setq comint-preoutput-filter-functions nil)
  ;; (add-hook 'comint-preoutput-filter-functions
  ;;           'sql-interactive-remove-continuation-prompt nil t)
  (add-hook 'comint-preoutput-filter-functions
  		    'sql-add-newline-first t t)
  (sqlup-mode)
  (sql-rename-buffer)
  (repl-mode +1)
  )

(add-hook 'sql-mode-hook                'config-mode-sql)
(add-hook 'sql-interactive-mode-hook    'config-mode-sql-interactive)


;; --------------------------------------------------------------------------
;; Frame Settings 
;; --------------------------------------------------------------------------
(add-to-list
 'display-buffer-alist
 '("\\*SQL.*\\*"
   (display-buffer-reuse-window display-buffer-pop-up-frame)
   (cascade .                   nil)
   (font-size .                 100)

   (pop-up-frame-parameters
    .
    ((top .                     20)
	 (left .                    810) 
	 (height .                  0.6) 
     (unsplittable .            t)
	 ))))

;; --------------------------------------------------------------------------
;; Key Binding
;; --------------------------------------------------------------------------
(define-many-keys sql-mode-map
  (kbd "<return>")	'reindent-then-newline-and-indent
  
  ;; ---------- Evaluation ----------
  [(shift return)]  'sql-eval
  [(M-return)]		'sql-eval-here

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")	'tab-to-tab-stop-magic
  ;; (kbd "<tab>")     'sql-fix-indent

  ;; ---------- Frame Switching ----------
  [f12]             'sql-switch-frame-process
  [S-f12]           'sql-connect
  [C-f12]			'sql-set-sqli-buffer
  [M-f12]           'sql-set-product


  ;; ---------- Help ----------  
  "\C-hf"           'sql-tables
  "\C-hF"           'sql-find-column
  ;; "\C-he"           'sql-explain
  "\C-hv"           'sql-describe
  "\C-hV"           'sql-describe-here
  "\C-hs"           'sql-show-table
  "\C-ho"           'sql-inspect-table
  )

(define-many-keys sql-interactive-mode-map
  ;; ---------- Input / Prompt Scrolling ----------
  [C-up]			'comint-previous-prompt
  [C-down]          'comint-next-prompt
  [up]              'comint-previous-input
  [down]			'comint-next-input
  [S-C-up]          'previous-line
  [S-C-down]		'next-line

  ;; ---------- Frame Switching ----------
  [(f12)]           'sql-switch-frame-script
  [S-f12]           'sql-reconnect


  ;; ---------- Completion ----------
  ;; (kbd "<tab>")     'completion-at-point

  ;; ---------- Help ----------
  [(f1)]            '(lambda ()
                       (interactive)
				       (google-query-at-point
                        t
                        (format "SQL %s " sql-product)))
  [(S-f1)]          '(lambda ()
			           (interactive)
			           (google-query-at-point
                        nil
                        (format "SQL %s " sql-product)))
  (kbd "C-h w")   	'(lambda ()
				       (interactive)
				       (google-query-at-point
                        nil
                        (format "SQL %s " sql-product)))

  "\C-hf"           'sql-tables
  "\C-hF"			'sql-find-column
  ;; "\C-he"              'sql-explain
  "\C-hv"           'sql-describe
  "\C-hs"           'sql-show-table
  "\C-ho"           'sql-inspect-table
  )


;; --------------------------------------------------------------------------
;; Product Features
;; --------------------------------------------------------------------------

(sql-set-product-feature
 'ms :prompt-regexp "^[0-9]*>") ;existing line

(sql-set-product-feature
 'ms :prompt-cont-regexp "^[0-9]*>") ;new line

(defun sql-load-help-queries (dir)
  (--each
      (f-entries dir
                 (lambda (file) (f-ext? file "sql")) t)
    (sql-set-product-feature
     (intern (f-filename (f-dirname it)))
     (intern (s-concat ":query-" (f-base it)))
     (f-read it 'utf-8))
    (message "Loaded %s for %s"
             (f-filename it)
             (s-upcase (f-filename (f-dirname it))))))

(when-let
    ((dir (config-get :applications :sql :query-dir)))
  (sql-load-help-queries dir))


;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------

;; ---------- Capitalization ----------
(defun sqlup-capitalize-keywords ()
  (interactive)
  (if mark-active
      (call-interactively #'sqlup-capitalize-keywords-in-region)
    (sqlup-capitalize-keywords-in-buffer)))

(defun sqlup-capitalize-keywords-in-line ()
  (interactive)
  (sqlup-capitalize-keywords-in-region
   (line-beginning-position) (line-beginning-position 2)))


;; ---------- Indentation ----------
(defun sqlind-indent-logic (syntax base-indentation)
  "Indents lines starting with AND, OR, NOT"
  (save-excursion
    (back-to-indentation)
    (if (looking-at "and\\|or\\|not")
        (+ base-indentation sqlind-basic-offset)            
      base-indentation)))

(defun sqlind-indent-lineup-previous-comment (syntax base-indentation)
  "Return the indentation for a previous comment.

If we start a line comment (--) and the previous line also has a line
comment, we line up the two comments.  Otherwise nothing (return
BASE-INDENTATION)."
  (save-excursion
    (back-to-indentation)
    (if (and (looking-at "\\s *--")
             (progn
               (forward-line -1)
               (re-search-forward "--" (line-end-position) t)))
        (progn
          (goto-char (match-beginning 0))
          (current-column))
      base-indentation)))

(defun sqlind-set-indentation ()
  (interactive)
  (setq
   sqlind-indentation-offsets-alist
   `((syntax-error                  sqlind-report-sytax-error)
     
     ;; top most indent level
     (toplevel                      0)

     ;; ------------- COMMENTS -------------
     ;; -- comment     <- comment-start
     ;; /*             <- comment-start
     ;;   comment      <- comment-continuation
     ;;  */            <- comment-continuation
     (comment-start                 sqlind-use-previous-line-indentation
                                    sqlind-indent-lineup-previous-comment)
     (comment-continuation          sqlind-indent-comment-continuation)


     ;; line is inside a string, ANCHOR - start of the string
     (in-string                     sqlind-report-runaway-string)
     (string-continuation           0)


     ;; line is after a DECLARE keyword
     (declare-statement             +)

     ;; inside a CREATE statement
     (create-statement              +)

     ;; inside a PROCEDURE of FUNCTION definition
     (defun-start                   +)

     ;; line is just after a label.
     (labeled-statement-start       0)
     

     ;; ------------- WITH -------------
     ;; +A+ within WITH clause, but before the main SELECT clause          
     (with-clause                   sqlind-use-anchor-indentation)
     ;; after WITH but before a CTE declaration
     (with-clause-cte               sqlind-use-anchor-indentation
                                    +)
     ;; after WITH but before a CTE declaration (continued line)
     (with-clause-cte-cont          sqlind-use-anchor-indentation
                                    +)

     ;; ------------- SELECT -------------          
     ;; +A+ line is inside a select statement, right before one of its
     ;; clauses (from, where, order by, etc). 
     (select-clause                 0)

     ;; between SELECT and FROM
     ;; before a column declaration
     ;; ANCHOR = start of select statement itself
     (select-column                 sqlind-indent-select-column)


     ;; after SELECT but before next clause (e.g. FROM)
     ;; inside column definition
     ;; ANCHOR = start of select statement itself
     (select-column-continuation    +
                                    sqlind-indent-select-column
                                    sqlind-lone-semicolon)
     
     ;; ------------- FROM -------------
     ;; FROM               <- select-clause
     ;;   T1               <- select-table
     ;;   JOIN             <- select-table
     ;;   T3 ON            <- select-table
     ;;     c3 = c2        <- select-table-continuation
     ;;                        ^ ANCHOR - table definition
     ;;       AND          <- select-join-condition
     ;;                        ^ ANCHOR - JOIN statement
     ;;     d3 = d1        <- select-table-continuation
     (select-table                  sqlind-indent-select-table)
     (select-table-continuation     sqlind-indent-select-table
                                    +
                                    sqlind-lone-semicolon)
     (select-join-condition         sqlind-indent-select-table
                                    ++)      
     
     ;; ------------- WHERE / GROUP BY / ORDER BY -------------
     (in-select-clause              +
                                    sqlind-indent-logic           
                                    sqlind-lone-semicolon)

     ;; ------------- CASE -------------
     ;; CASE           <- select-column
     ;;   WHEN x = 2   <- case-clause
     ;;                    ^ ANCHOR - CASE clause
     ;;   AND          <- case-clause-item-cont
     ;;   z = 3        <- case-clause-item-cont
     ;;     THEN 1 +   <- case-clause-item
     ;;                    ^ ANCHOR - CASE clause
     ;;     3          <- case-clause-item-cont
     ;;                    ^ ANCHOR - case keyword continuation of
     ;;   ELSE 2       <- case-clause
     ;; END            <- block-end
     (case-clause                   +)          
     (case-clause-item              0)
     (case-clause-item-cont         sqlind-use-anchor-indentation
                                    +)

     ;; line begins with a statement that starts a block
     (block-start                   0)
     ;; the line contains an END statement
     (block-end                     +)
     ;; line is inside a block construct
     (in-block                      +)
     ;; line is inside a block started by a BEGIN statement
     (in-begin-block                +)

     ;; inside a package definition
     (package                       +)
     ;; inside a package body
     (package-body                  +)          

     ;; a statement which starts on a previous line.
     (statement-continuation        +)

     ;; ------------- BRACKETS -------------
     ;; SUM(       <- select-column
     ;;   x1 +     <- nested-statement-open
     ;;     x2     <- nested-statement-continuation
     ;;   ) AS y   <- nested-statement-close
     (nested-statement-open         sqlind-use-anchor-indentation
                                    +)
     (nested-statement-continuation sqlind-use-anchor-indentation
                                    sqlind-indent-logic
                                    +)
     (nested-statement-close        sqlind-use-anchor-indentation)

     ;; (nested-statement-continuation sqlind-lineup-to-anchor
     ;;                                sqlind-indent-logic
     ;;                                1)
     ;; (nested-statement-close        +)

     ;; ------------- DELETE -------------          
     (delete-clause                 0)
     ;; inside a delete CLAUSE (i.e. DELETE FROM or WHERE)
     (in-delete-clause              +
                                    sqlind-indent-logic
                                    sqlind-lone-semicolon)

     ;; ------------- INSERT -------------
     (insert-clause                0)
     ;; inside the insert CLAUSE (i.e. INSERT INTO or VALUES)
     (in-insert-clause              +
		                            sqlind-indent-logic
                                    sqlind-lone-semicolon)
     
     ;; ------------- UPDATE -------------
     (update-clause                 0)
     ;; inside an update CLAUSE (i.e. UPDATE, SET, or WHERE)
     (in-update-clause              +
                                    sqlind-indent-logic
                                    sqlind-lone-semicolon)
     )))

(defadvice sqlind-indent-line (after sqlup-capitalize activate compile)
  "Capitalize keywords when indenting"
  (sqlup-capitalize-keywords-in-line))


;; ---------- REPL ----------
   ;; repl-function-eval           #'sql-eval
   ;; repl-function-eval-insert    #'sql-eval-insert
   ;; repl-function-select         #'sql-select-repl
   ;; repl-function-create         #'sql-create-repl

(defun marginalia-annotate-sql-connection (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (cdr (assoc-string cand sql-connection-alist t)))
    
    (let ((server
           (-last-item
            (--first (eq (car it) 'sql-server) sym))))
      (marginalia--fields
       (server :truncate 1.0 :face 'marginalia-documentation)))))

(add-to-list 'marginalia-annotator-registry
             '(sql marginalia-annotate-sql-connection builtin none))

(defun sql-connections-completion (str pred flag)
  (pcase flag
    ('metadata
     `(metadata
       (category . sql)
       ;; (annotation-function . ,(lambda (s) "xxx"))
       ))
    (_
     (all-completions str (mapcar #'car sql-connection-alist) pred))))

(defun sql-read-connection-filtered (orig-fun prompt initial default)
  "Read a connection name."
  (let ((completion-ignore-case t)
	    (sql-connections
	     (remove
		  nil
		  (mapcar
		   #'(lambda (c) (when
				        (eq (cadadr (assoc `sql-product
								           (cdr c))) sql-product)
				      (car c)))
		   sql-connection-alist))))
    ))



;; Filters connections by product
(defun sql-read-connection-filtered (orig-fun prompt initial default)
  "Read a connection name."
  (let ((completion-ignore-case t)
	    (sql-connections
	     (remove
		  nil
		  (mapcar
		   #'(lambda (c) (when
				        (eq (cadadr (assoc `sql-product
								           (cdr c))) sql-product)
				      (car c)))
		   sql-connection-alist))))
    (completing-read
	 prompt
	 sql-connections
	 nil t initial 'sql-connection-history default)))
;; (advice-add 'sql-read-connection :around #'sql-read-connection-filtered)

;; Restricts to only SQLi buffers
(defun sql-set-sqli-buffer-filtered (orig-fun &rest args)
  (let ((icicle-buffer-complete-fn (list)))
    (dolist ($buf (buffer-list (current-buffer)))
	  (with-current-buffer $buf
	    (when (eq major-mode 'sql-interactive-mode)
		  (add-to-list 'icicle-buffer-complete-fn
				       (buffer-name $buf)))))
    (apply orig-fun args)
    )
  )
(advice-add 'sql-set-sqli-buffer
		    :around #'sql-set-sqli-buffer-filtered)

;; Wrap send-string with saving excursion so the current frame doesn't lose
;; focus. This appears to be used by all all higher level SEND functions.
;; (advice-add 'sql-send-string :around
;; 		    #'(lambda (orig-fun &rest args)
;; 			    (progn ; FIXME was save-frame-excursion  (apply orig-fun args))))


;; Fix the first line of the output
(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
  ;; (message "\norg:\n%s"
  ;; 		 output)
  ;; (message "new:\n%s"
  ;; 		 (sql-interactive-remove-continuation-prompt output))
  
  ;; (concat "\n" output)
  (sql-interactive-remove-continuation-prompt output))

(defun test-sql-interactive-remove-continuation-prompt (oline)
  (when comint-prompt-regexp
    (save-match-data
      (let (prompt-found
		    last-nl)

        ;; Add this text to what's left from the last pass
        (setq oline (concat sql-preoutput-hold oline)
              sql-preoutput-hold "")

        ;; If we are looking for multiple prompts
        (when (and (integerp sql-output-newline-count)
                   (>= sql-output-newline-count 1))
          ;; Loop thru each starting prompt and remove it
          (let ((start-re (sql-starts-with-prompt-re)))
            (while (and
				    (not (string= oline ""))
				    (> sql-output-newline-count 0)
				    (string-match start-re oline))
              (setq
			   oline (replace-match "" nil nil oline)
			   sql-output-newline-count (1- sql-output-newline-count)
			   prompt-found t)))
          
          ;; If we've found all the expected prompts, stop looking
		  (message "%s" sql-output-newline-count)

          (if (= sql-output-newline-count 0)
              (setq
			   sql-output-newline-count nil)

            ;; Still more possible prompts, leave them for the next pass
            (setq
		     sql-preoutput-hold oline
		     oline ""))
		  )

	    (message "%s" prompt-found)
        ;; If no prompts were found, stop looking
        (unless prompt-found
          (setq sql-output-newline-count nil
                oline (concat oline sql-preoutput-hold)
                sql-preoutput-hold ""))

        ;; Break up output by physical lines if we haven't hit the final prompt
        (unless (and
			     (not (string= oline ""))
			     (string-match (sql-ends-with-prompt-re) oline)
			     (>= (match-end 0) (length oline)))
		  (message "Break up")
          (setq last-nl 0)
          (while (string-match "\n" oline last-nl)
            (setq last-nl (match-end 0)))
          (setq sql-preoutput-hold (concat (substring oline last-nl)
                                           sql-preoutput-hold)
                oline (substring oline 0 last-nl)))
	    )))
  oline)


(defun sql-reconnect ()
  "Reconnects to database using the associated sqli buffer"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-con)))
    (cond
	 (func     
	  (sql-send-string
	   (format func
			   (sql-default-value 'sql-user)
			   (sql-default-value 'sql-password)
			   (sql-default-value 'sql-database)))
	  (sql-eval-init))
	 (t
	  (message "no :func-con defined for product %s" sql-product)))))

;; (defun sql-output-here (func)
;;   (interactive)
;;   (cl-letf (((symbol-function 'sql-send-string)
;; 		     #'(lambda (str)
;;   			     (sql-redirect
;;   			      (sql-find-sqli-buffer)
;;   			      str
;;   			      " *SQL Echo Area*"))))
;;     ;; (advice-add 'sql-send-string :around
;;     ;; 			 #'(lambda (orig-fun &rest args)
;;     ;; 				(sql-redirect
;;     ;; 				 (sql-find-sqli-buffer)
;;     ;; 				 args
;;     ;; 				 " *SQL Echo Area*")))
;;     ;; (let ((comint-preoutput-filter-functions
;;     ;; 		 '(sql-interactive-remove-continuation-prompt)))
;; 	(funcall func)
;; 	;; )
;;     ;; (advice-remove 'sql-send-string 
;;     ;; 			    #'(lambda (orig-fun &rest args)
;;     ;; 				   (sql-redirect
;;     ;; 				    (sql-find-sqli-buffer)
;;     ;; 				    args			  
;;     ;; 				    " *SQL Echo Area*")))
;;     ))

;; (defun sql-eval-here ()
;;   (interactive)
;;   (sql-output-here
;;    '(lambda ()
;; 	  (sql-eval)
;; 	  ;; since the eval auto increments to the next line
;; 	  (previous-non-blank-line)
;; 	  (end-of-line)
;; 	  (newline))))

(defun sql-select-repl ()
  (when (sql-find-sqli-buffer sql-product)
	(sql-set-sqli-buffer)))

(defun sql-eval ()
  "Evaluates SQL code."
  (interactive)  
  (unless (and transient-mark-mode mark-active)
    (mark-paragraph))
  (sql-send-region (region-beginning) (region-end))
  (deactivate-mark)
  (forward-paragraph)
  (next-non-blank-line))


(defun sql-eval-file (f)
  "Evaluates an SQL file."
  (interactive "fEnter File: ")
  (sql-send-string
   (get-string-from-file f)))

(defun sql-eval-init ()
  "Evaluates SQL Init File."
  (interactive)
  (let ((init-file (sql-get-product-feature sql-product :init-file)))
    (when init-file
	  (sql-eval-file init-file))))     

(advice-add 'sql-product-interactive :after
		    #'(lambda (&rest args) (sql-eval-init)))



;; ---------- Frame Commands ---------- ;;
(defun sql-switch-frame-process ()
  "Switch to associated process, associate with one, or create one."
  (interactive)
  (cond
   ;; Does current buffer have an associated process?
   (sql-buffer
    ;; Yes -> raise and select
    (display-buffer sql-buffer))
   ;; No -> are there processes running?
   ((sql-find-sqli-buffer sql-product)
    ;; Yes -> associate -> raise 
    (sql-set-sqli-buffer))
   (t
    ;; No -> create one -> associate -> raise 
    (call-interactively 'sql-connect)))
  
  (switch-to-buffer-other-frame sql-buffer)
  (end-of-buffer-all))

(defun sql-raise-frame-process ()
  (progn ; FIXME was save-frame-excursion 
    (raise-frame
     (get-frame sql-buffer))))

(defun sql-switch-frame-script ()
  "Switch to most recent script buffer."
  (interactive)
  (let ((loc-proc-name (buffer-name))
	    (blist (cdr (buffer-list))))
    (while (and blist
			    (with-current-buffer (car blist)
			      (not (and
				        (equal major-mode 'sql-mode)
				        (equal loc-proc-name sql-buffer)))))
	  (pop blist))
    (if blist
	    (display-buffer (car blist) t)
	  (message "Found no SQL associated with process %s"
			   loc-proc-name))))


;; -----------------------------------------------------------------------------
;; Help Functions
;;	- Vary by product / in progress
;; -----------------------------------------------------------------------------
(defun sql-table-at-point ()  
  (let* ((table-start
		  (save-excursion
		    (or
		     (search-backward-regexp "[^._[:alnum:]]" nil t)
		     (1- (point-min)))))
	     (table-end
		  (save-excursion
		    (or
		     (search-forward-regexp "[^._[:alnum:]]" nil t)
		     (1+ (point-max))))))
    (buffer-substring-no-properties
	 (1+ table-start) (1- table-end))))

(defun sql-describe ()
  "Describe the current table"
  (interactive)
  (if-let ((query (sql-get-product-feature
                   sql-product :query-describe-table))
           (table (upcase (sql-table-at-point))))
      (sql-send-string (s-lex-format query))
	(message "no :query-describe-table defined for product %s"
             sql-product)))

(defun sql-describe-here ()
  (interactive)
  (sql-output-here
   '(lambda ()
	  (sql-describe)
	  (end-of-line)
	  (newline))))

(defun sql-show-table ()
  "Lists the top elements of a table"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-show-table)))
    (if func     
	    (sql-send-string (replace-regexp-in-string
					      "TABLE"
					      (upcase (sql-table-at-point))
					      func))
	  (message "no :func-show-table defined for product %s" sql-product))))

(defun sql-inspect-table ()
  "Lists some table info"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-inspect-table)))
    (if func     
	    (sql-send-string (replace-regexp-in-string
					      "TABLE_PATTERN"
					      (upcase (sql-table-at-point))
					      func))
	  (message "no :func-inspect-table defined for product %s" sql-product))))


(defun sql-tables (table-pattern owner-pattern)
  "Lists tables (or views) that match table / schema pattern"
  (interactive "sTable: \nsOwner/Schema: ")
  (let ((func (sql-get-product-feature sql-product :func-list-tables)))
    (if func
        (progn
	      (sql-send-string
	       (replace-regexp-in-string
	        "OWNER_PATTERN"
	        (upcase owner-pattern)
	        (replace-regexp-in-string
		     "TABLE_PATTERN"
		     (upcase table-pattern)
		     func)))
	      (display-buffer sql-buffer))
	  (message "no :func-list-tables defined for product %s" sql-product))))

(defun sql-find-column (pattern)
  "Find all tables / views that contain column"
  (interactive "sPattern: ")
  (let ((func (sql-get-product-feature sql-product :func-find-column)))
    (if func     
	    (progn ; FIXME was save-frame-excursion 
	      (sql-send-string
	       (replace-regexp-in-string
	        "PATTERN"
	        (upcase pattern)
	        func))
	      (display-buffer sql-buffer))
	  (message "no :func-find-column defined for product %s" sql-product))))

(defun sql-user-tables (pattern)
  "Show all USER tables"
  (interactive "sPattern: ")
  (let ((func (sql-get-product-feature sql-product :func-user-tables)))
    (if func     
	    (progn ; FIXME was save-frame-excursion 
	      (sql-send-string
	       (replace-regexp-in-string
	        "PATTERN"
	        (upcase pattern)
	        func))
	      (display-buffer sql-buffer))
	  (message "no :func-user-tables defined for product %s" sql-product))))

(defun sql-user-functions ()
  "Shows all USER functions"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-user-functions)))
    (if func     
	    (progn ; FIXME was save-frame-excursion 
	      (sql-send-string func)
	      (display-buffer sql-buffer))
	  (message "no :func-user-functions defined for product %s" sql-product))))

(defun sql-last-error ()
  "Shows last error"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-last-error)))
    (if func     
	    (progn ; FIXME was save-frame-excursion 
	      (sql-send-string func)
	      (display-buffer sql-buffer))
	  (message "no :func-last-error defined for product %s" sql-product))))

(defun sql-explain ()
  "Explain plan of code"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-explain)))
    (if func
	    (progn ; FIXME was save-frame-excursion  
	      (sql-send-string (replace-regexp-in-string
					        "REGION"
					        (get-region-as-string)
					        func)))
	  (message "no :func-explain defined for product %s" sql-product))))


;; Suppress Abbrev in Comments 
;; (defun sql-mode-abbrev-expand-function (expand)
;;   (if (or
;; 	   (nth 3 (syntax-ppss))		; inside string.
;; 	   (nth 4 (syntax-ppss))		; inside comment
;; 	   )
;; 	  ;; Use the text-mode abbrevs.
;; 	  (let ((local-abbrev-table text-mode-abbrev-table))
;; 	    (funcall expand))
;;     ;; Else performs normal expansion.
;;     (funcall expand)
;;     )
;;   )

;; TODO: Does this still need to be used?
;; Fix SQL Highlighting + Rainbow Delimiters
;;	sql-highlight-product overrides rainbow-delimiters so we need to reapply it
;;	afterwords if it was on
;; (advice-add 'sql-highlight-product :around
;; 		    '(lambda (func)
;; 			   (let ((rainbow-state rainbow-delimiters-mode))
;; 			     (funcall func)
;; 			     (rainbow-delimiters-mode rainbow-state))))


;; ------------------------------------------------------------------------- ;;
;; Syntax Highlighting
;; ------------------------------------------------------------------------- ;;
(font-lock-add-keywords
 'sql-mode
 '(
   ;; ---------- Numbers ---------- ;;
   ("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b"
    .
    'font-lock-number-face)
   
   ;; ---------- Logic Operators ---------- ;;
   ("\\(?:!=\\|<[=>]\\|>=\\|[<=>]\\)\\|\\(\\b\\(all\\|and\\|any\\|between\\|
exists\\|in\\|like\\|not\\|or\\|some\\)\\b\\)"
    .
    'font-lock-relation-operator-face)
   
   ;; ---------- Defined Variables ---------- ;;
   ("&?&\\(?:\\sw\\|\\s_\\)+[.]?"
    0
    'font-lock-variable-name-face t)))

(font-lock-add-keywords
 'sql-interactive-mode
 '(
   ;; ---------- Numbers ---------- ;;
   ("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b"
    .
    'font-lock-number-face)))

(sql-add-product-keywords
 'oracle
 `(
   ;; Additional Functions
   (,(regexp-opt
      '("appendchildxml" "cardinality" "cluster_id" "cluster_probability"
        "cluster_set" "collect" "corr_k" "corr_s" "cv" "deletexml" "empty_blob"
        "extractxml" "feature_id" "feature_set" "feature_value" "grouping"
        "insertchildxml" "insertxmlbefore" "instr2" "instr4" "instrb" "instrc"
        "iteration_number" "length2" "length4" "lengthb" "lengthc" "lnnvl" "log"
        "median" "nanvl" "nchr" "ora_hash" "powermultiset_by_cardinality"
        "prediction_cost" "prediction_details" "prediction_probability"
        "prediction_set" "presentnnv" "presentv" "previous" "ref" "remainder"
        "stats_binomial_test" "stats_crosstab" "stats_f_test" "stats_ks_test"
        "stats_mode" "stats_mw_test" "stats_one_way_anova" "stats_t_test_indep"
        "stats_t_test_indepu" "stats_t_test_one" "stats_t_test_paired"
        "stats_wsr_test" "substr2" "substr4" "substrb" "substrc"
        "timestamp_to_scn" "to_binary_double" "to_binary_float" "value" "xmlparse"
        "xmlpi" "xmlquery" "xmlroot" "xmlserialize" "xmltable" "trunc"))
    .
    'font-lock-builtin-face)
   ;; Package Functions
   (,(concat
      (regexp-opt
       '("DBMS_SESSION" "DBMS_UTILITY" "DBMS_AQ_EXP_INDEX_TABLES"
	     "DBMS_AQ_EXP_SUBSCRIBER_TABLES" "DBMS_RESOURCE_MANAGER"
	     "DBMS_ITRIGGER_UTL" "DBMS_SUMMARY" "DBMS_REFRESH_EXP_SITES"
	     "DBMS_TRACE" "UTL_COLL" "OWA_IMAGE" "DBMS_AW_EXP" "DBMS_JAVA"
	     "GET_ERROR$" "GENMETADATAPROVIDERINTERFACE" "DBMS_SCHED_PROGRAM_EXPORT"
	     "DBMS_SCHED_WINGRP_EXPORT" "DBMS_SCHED_SCHEDULE_EXPORT"
	     "DBMS_SCHEDULER" "KUPW$WORKER" "DBMS_DIMENSION" "DBMS_REPCAT_EXP"
	     "DBMS_FILE_GROUP_IMP" "DBMS_CDC_ISUBSCRIBE" "UTL_DBWS" "DBMS_SQLDIAG"
	     "DBMS_SCHED_CREDENTIAL_EXPORT" "DBMS_SQLTUNE_UTIL2"
	     "DBMS_PREDICTIVE_ANALYTICS" "DBMS_CUBE_ADVISE_SEC" "DIUTIL"
	     "DBMS_JAVA_TEST" "UTL_INADDR" "UTL_URL" "UTL_ENCODE" "DBMS_SQL"
	     "DBMS_LOB" "DBMS_TRANSFORM_EXIMP" "DBMS_AQ_EXP_SIGNATURE_TABLES"
	     "OWA_SEC" "OWA" "OWA_TEXT" "DBMS_DEBUG_JDWP" "DBMS_DEBUG_JDWP_CUSTOM"
	     "DBMS_REPUTIL2" "DBMS_LCR" "GENMDMPROPERTYIDCONSTANTS" "OWA_MATCH"
	     "DBMS_STREAMS_PUB_RPC" "DBMS_CDC_IMPDP" "DBMS_COMPRESSION"
	     "DBMS_CUBE_UTIL" "UTL_FILE" "DBMS_STATS" "DBMS_TYPES"
	     "DBMS_AQ_EXP_QUEUE_TABLES" "DBMS_RESOURCE_MANAGER_PRIVS"
	     "DBMS_RMGR_PACT_EXPORT" "DBMS_XRWMV" "URIFACTORY" "DBMS_METADATA"
	     "DBMS_REPCAT_INSTANTIATE" "DBMS_LOGREP_IMP" "GENMDMOBJECTIDCONSTANTS"
	     "GENFUNCTIONIDCONSTANTS" "DBMS_SCHED_WINDOW_EXPORT"
	     "DBMS_SCHED_CHAIN_EXPORT" "DBMS_INDEX_UTL"
	     "DBMS_AQ_EXP_CMT_TIME_TABLES" "KUPCC" "DBMS_FBT" "DBMS_LDAP_UTL"
	     "DBMS_DB_VERSION" "DBMS_AW_XML" "DBMS_XQUERYINT" "DBMS_JDM_INTERNAL"
	     "DBMS_CUBE" "DBMS_CUBE_EXP" "UTL_GDK" "DBMS_PCLXUTIL" "DBMS_RULE_ADM"
	     "DBMS_RULE_EXP_RULE_SETS" "DBMS_RULE_EXP_RULES" "DBMS_DEBUG" "PBSDE"
	     "DBMS_REFRESH_EXP_LWM" "UTL_REF" "PRVT_EGUTL" "OWA_CACHE"
	     "DBMS_STREAMS" "DBMS_LOGREP_EXP" "GENINTERRUPTABLEINTERFACE"
	     "GENDATABASEINTERFACE" "GENSERVERINTERFACE"
	     "GENDEFINITIONMANAGERINTERFACE" "GENDATAPROVIDERINTERFACE"
	     "DBMS_STAT_FUNCS_AUX" "DBMS_ADVISOR" "UTL_COMPRESS" "KUPF$FILE"
	     "DBMS_SQLTUNE" "DBMS_RESULT_CACHE_API" "DBMS_LOBUTIL"
	     "DBMS_SCHED_ATTRIBUTE_EXPORT" "PRIVATE_JDBC" "UTL_IDENT"
	     "DBMS_AUTO_TASK" "DBMS_XS_SESSIONS" "DBMS_DM_MODEL_IMP"
	     "PRVT_COMPRESSION" "DBMS_HS_PARALLEL" "DBMS_CUBE_LOG" "JVMRJBC"
	     "UTL_HTTP" "DBMS_DDL" "DBMS_SPACE" "DBMS_EXPORT_EXTENSION" "DBMS_RULE"
	     "DBMS_RULE_IMP_OBJ" "DBMS_AQ_EXP_QUEUES" "DBMS_AQJMS" "DBMS_RMIN"
	     "DBMS_RMGR_PLAN_EXPORT" "DBMS_RMGR_GROUP_EXPORT" "DBMS_EPGC"
	     "OWA_OPT_LOCK" "DBMS_AW" "DBMS_CRYPTO_TOOLKIT" "DBMS_XMLQUERY"
	     "GENCURSORMANAGERINTERFACE" "DBMS_ERRLOG" "DBMS_PREPROCESSOR"
	     "DBMS_DATAPUMP" "DBMS_SUM_RWEQ_EXPORT" "DBMS_FREQUENT_ITEMSET"
	     "DBMS_CDC_DPUTIL" "SQLJUTL2" "DBMS_JVM_EXP_PERMS" "DBMS_EPG" "DBMS_XA"
	     "DBMS_ADDM" "DBMS_SPM" "DBMS_DATA_MINING_TRANSFORM" "ODM_UTIL"
	     "DBMS_AW_STATS" "DBMS_NETWORK_ACL_UTILITY" "DBMS_XQUERY" "STANDARD"
	     "DBMS_TRANSACTION" "DBMS_ROWID" "DBMS_DESCRIBE" "DBMS_ODCI"
	     "DBMS_ZHELP_IR" "DBMS_RULEADM_INTERNAL" "DBMS_RANDOM"
	     "DBMS_CDC_SUBSCRIBE" "OWA_COOKIE" "WPG_DOCLOAD" "SQLJUTL"
	     "GENMDMCLASSCONSTANTS" "DBMS_SCHED_JOB_EXPORT"
	     "DBMS_SCHED_EXPORT_CALLOUTS" "DBMS_STAT_FUNCS" "DBMS_AQ_INV"
	     "DBMS_XMLSTORE" "UTL_LMS" "KUPM$MCP" "DBMS_RULE_EXP_UTLI"
	     "DBMS_FILE_GROUP_EXP" "DBMS_LDAP" "DBMS_SQLPA" "DBMS_REPORT"
	     "DBMS_AQ_EXP_DEQUEUELOG_TABLES" "DBMS_SCHED_FILE_WATCHER_EXPORT"
	     "DBMS_DM_MODEL_EXP" "DBMS_STANDARD" "DBMS_PICKLER" "DBMS_XPLAN"
	     "DBMS_APPLICATION_INFO" "DBMS_OUTPUT" "DBMS_JOB" "ODCICONST"
	     "DBMS_RULE_EXP_EV_CTXS" "DBMS_AQ_EXP_TIMEMGR_TABLES" "DBMS_SNAPSHOT"
	     "DBMS_REFRESH" "DBMS_SNAPSHOT_UTL" "HTP" "OWA_UTIL" "JAVA_XA"
	     "DBMS_XMLSAVE" "GENCONNECTIONINTERFACE" "GENDATATYPEIDCONSTANTS"
	     "DBMS_ASSERT" "DBMS_WARNING" "UTL_NLA" "DBMS_PROFILER"
	     "DBMS_METADATA_DIFF" "PRVT_REPORT_TAGS" "DBMS_PARALLEL_EXECUTE"
	     "KUPU$UTILITIES" "DBMS_CUBE_ADVISE" "UTL_RAW" "PLITBLM" "UTL_TCP"
	     "UTL_SMTP" "DBMS_PSP" "DBMS_AQ_EXP_HISTORY_TABLES"
	     "DBMS_AQ_IMP_INTERNAL" "OUTLN_EDIT_PKG" "DBMS_OBFUSCATION_TOOLKIT"
	     "DBMS_XMLGEN" "WPIUTL" "OWA_CUSTOM" "HTF" "OWA_PATTERN" "DBMS_REPUTIL"
	     "DBMS_OFFLINE_RGT" "DBMS_REPCAT_RGT_EXP" "GENPARAMETERIDCONSTANTS"
	     "DBMS_SCHED_CLASS_EXPORT" "UTL_I18N" "DBMS_CDC_EXPDP" "DBMS_CDC_EXPVDP"
	     "UTL_MATCH" "DBMS_EDITIONS_UTILITIES" "DBMS_DATA_MINING")
       t) ".[a-z0-9_]+")
    .
    'font-lock-builtin-face)))


(provide 'config-sql)
;;; CONFIG-SQL.EL ends here



;; sql-prompt-regexp

;; (let ((comint-preoutput-filter-functions
;; 	  '(sql-interactive-remove-continuation-prompt)))
;;   (sql-redirect
;;    (sql-find-sqli-buffer)
;;    "
;; describe ADL_DEV3.NCRE_RULES;"
;;    " *SQL Echo Area*"))


;; (get-buffer "*SQL: <edwrpt.world-SCI>*")


;; (save-excursion
;;   ;; switch to SQLi buffer to get the prompt regex
;;   (set-buffer (sql-find-sqli-buffer))
;;   (let ((start-re (sql-starts-with-prompt-re))
;; 		echo-size)

;; 	 ;; switch to the echo to replace
;; 	 (set-buffer (get-buffer-create " *SQL Echo Area*"))
;; 	 (setq echo-size (1+ (buffer-size)))
;; 	 (beginning-of-buffer)
;; 	 (while (and (re-search-forward start-re nil t)
;; 			   (> echo-size
;; 				 (setq echo-size (buffer-size))))
;; 	   (replace-match ""))

;; 	 ;; delete leading empty lines
;; 	 (beginning-of-buffer)
;; 	 (while (re-search-forward "^[[:space:]]*$" (line-end-position) t)
;; 	   (kill-whole-line))

;; 	 ;; delete trailing empty lines
;; 	 (delete-trailing-whitespace)


;; 	 ;; delete end of buffer line
;; 	 (end-of-buffer)
;; 	 (beginning-of-line)
;; 	 (when (= (point) (point-max))
;; 	   (delete-char -1))

;; 	 ;; append comment string to each line
;; 	 (beginning-of-buffer)
;;     (cl-loop repeat
;;   	 	   (count-lines (point-min) (point-max))
;;   	 	   do
;;   	 	   (insert "-- ")
;;   	 	   (forward-line 1))
;; 	 )
;;   )

;; ;; copy buffer over to SQL file
;; (insert-buffer-substring " *SQL Echo Area*")
;; (next-non-blank-line)
;; )



;; (defun sql-fix-indent ()
;;   "Fixes indents for a whole paragraph. Pretty much all one should need."
;;   (interactive)
;;   (save-excursion
;;     (progn
;;       (mark-paragraph)
;;       (call-interactively 'indent-region))))


;; (defun sql-eval ()
;;   "Evaluates SQL code."
;;   (interactive)
;;   ;; Pre Eval
;;   (unless sql-buffer
;;     (if (null (sql-find-sqli-buffer))
;; 	   ;; Connect
;; 	   (call-interactively 'sql-connect)
;; 	 ;; else Set
;; 	 (sql-set-sqli-buffer)))

;;   ;; Evaluate depending on mark mode
;;   (if (and transient-mark-mode mark-active)
;;       (progn
;; 	   (call-interactively 'sql-send-region)
;; 	   (deactivate-mark))
;;     (progn
;; 	 ;; Send Paragraph code - fix for empty lines
;; 	 (let ((start (save-excursion
;; 				 (backward-paragraph)
;; 				 (next-non-blank-line)
;; 				 (point)))
;; 		  (end (save-excursion
;; 			    (forward-paragraph)
;; 			    (previous-non-blank-line)
;; 			    (end-of-line)
;; 			    (point))))
;; 	   (sql-send-region start end))
;; 	 (forward-paragraph)))
;;   (next-non-blank-line))


;; ---------- Frame Commands ---------- ;;
;; (defun sql-switch-frame-process ()
;;   "Switch to associated process, associate with one, or create one."
;;   (interactive)
;;   (cond
;;    ;; Does current buffer have an associated process?
;;    (sql-buffer
;;     ;; Yes -> raise and select
;;     (display-buffer sql-buffer))
;;    ;; No -> are there processes running?
;;    ((sql-find-sqli-buffer sql-product)
;;     ;; Yes -> associate -> raise 
;;     (sql-set-sqli-buffer))
;;    (t
;;     ;; No -> create one -> associate -> raise 
;;     (call-interactively 'sql-connect)))
  
;;   (switch-to-buffer-other-frame sql-buffer)
;;   (end-of-buffer-all))

;; (defun sql-raise-frame-process ()
;;   (progn ; FIXME was save-frame-excursion 
;;     (raise-frame
;;      (get-frame sql-buffer))))

;; (defun sql-switch-frame-script ()
;;   "Switch to most recent script buffer."
;;   (interactive)
;;   (let ((loc-proc-name (buffer-name))
;; 	    (blist (cdr (buffer-list))))
;;     (while (and blist
;; 			    (with-current-buffer (car blist)
;; 			      (not (and
;; 				        (equal major-mode 'sql-mode)
;; 				        (equal loc-proc-name sql-buffer)))))
;; 	  (pop blist))
;;     (if blist
;; 	    (display-buffer (car blist) t)
;; 	  (message "Found no SQL associated with process %s"
;; 			   loc-proc-name))))

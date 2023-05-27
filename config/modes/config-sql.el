;; -------------------------------------------------------------------------- ;;
;; SQL MODE                                                                   ;;
;; -------------------------------------------------------------------------- ;;
(require 'sql)
(require 's)
(require 'f)

(defconst sql-product-alist-default
  sql-product-alist
  "Back up of `sql-product-alist'")
(defconst sql-connection-alist-default
  sql-connection-alist
  "Back up of `sql-connection-alist'")

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

(defun config-sql-variables ()
  (interactive)
  
  (make-variable-buffer-local  'sql-product)
  (make-variable-buffer-local  'sql-connection)
  
  (let*
      ((sql-servers
        (-partition 2 (config-get :applications :sql :servers)))
       
       (products-original
        (-map #'car sql-product-alist-default))
       
       (products)
       
       (connections
        (--keep
         (when (memq (intern (plist-get (cadr it) :product))
                     products-original)
           (add-to-list 'products
                        (intern (plist-get (cadr it) :product)) t)
           (config-parse-sql-connection it))
         sql-servers)))

    (add-to-list 'products 'ansi t)
    
    (setq-default sql-connection-alist
                  connections)

    (setq-default sql-product-alist
                  (--map (assoc it sql-product-alist-default) products))

    (setq-default sql-product
                  (car products)))

  (setq-from-config sql-ms-program        :applications :sql :exe :ms)
  (setq-from-config sql-oracle-program    :applications :sql :exe :oracle))

(config-sql-variables)
(setq
 sql-send-terminator                    t
 sql-pop-to-buffer-after-send-region    nil
 sql-ms-options                         nil 
 sql-oracle-scan-on                     nil)

(advice-add 'sql-set-product :after
            (lambda (&rest args) (setq web-search-mode-prefix
                        (format "SQL %s " sql-product)))
            '((name . "sql-set-web-search-mode-prefix")))

;; Fix SQL Highlighting + Rainbow Delimiters
;;	sql-highlight-product overrides rainbow-delimiters so we need to reapply it
;;	afterwords if it was on
(advice-add 'sql-highlight-product :around
		     '(lambda (func)
			    (let ((rainbow-state rainbow-delimiters-mode))
			      (funcall func)
			      (rainbow-delimiters-mode rainbow-state)))
             '((name . "sql-rainbow-delimiters-mode")))


;; -------------------------------------------------------------------------- ;;
;; Product Features                                                           ;;
;; -------------------------------------------------------------------------- ;;
(sql-set-product-feature
 'ms :prompt-regexp "^[0-9]*>") ;existing line

(sql-set-product-feature
 'ms :prompt-cont-regexp "^[0-9]*>") ;new line

(defun sql-load-help-queries (dir)
  "Load helpful queries as product features.

Take each SQL file in DIR with the path <product>/<file>.sql and
loads it as a feature in `sql-product-alist' under the key
:query-<file>."
  (when-let*
      ((file-list
        (f-entries dir (lambda (file) (f-ext? file "sql")) t)))
    (--each file-list
      (sql-set-product-feature
       (intern (f-filename (f-dirname it)))
       (intern (s-concat ":query-" (f-base it)))
       (f-read it 'utf-8))
      (message "Loaded %s for %s"
               (f-filename it)
               (s-upcase (f-filename (f-dirname it)))))))

(sql-load-help-queries
 (f-expand (config-get-setting :applications :sql :query-directory) config-root))


;; -------------------------------------------------------------------------- ;;
;; Hooks                                                                      ;;
;; -------------------------------------------------------------------------- ;;
(defun config-mode-sql ()
  (interactive)
  (sqlind-minor-mode +1)
  (sqlind-set-indentation)
  (sqlup-mode +1)

  (repl-mode +1)
  
  (setq
   repl-interactive-mode        'sql-interactive-mode
   repl-function-eval           #'sql-eval
   repl-function-eval-insert    #'sql-eval-insert
   repl-function-set            #'sql-set-repl
   repl-function-create         #'sql-create-repl)

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
  ;; (add-hook 'comint-preoutput-filter-functions
  ;; 		    'sql-add-newline-first t t)
  (sqlup-mode)
  ;; (sql-rename-buffer)
  (repl-mode +1)
  )

(add-hook 'sql-mode-hook                'config-mode-sql)
(add-hook 'sql-interactive-mode-hook    'config-mode-sql-interactive)


;; -------------------------------------------------------------------------- ;;
;; Frame Settings                                                             ;;
;; -------------------------------------------------------------------------- ;;
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


;; -------------------------------------------------------------------------- ;;
;; Key Binding                                                                ;;
;; -------------------------------------------------------------------------- ;;
(define-keys sql-mode-map
  (kbd "<return>")	'reindent-then-newline-and-indent
  
  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")	'tab-to-tab-stop-magic

  ;; ---------- Frame Switching ----------
  [M-f12]           'sql-set-product

  ;; ---------- Help ----------  
  "\C-hf"           'sql-tables
  "\C-hF"           'sql-find-column
  "\C-hv"           'sql-describe
  "\C-hs"           'sql-show-table
  )

(define-keys sql-interactive-mode-map
  ;; ---------- Help ----------
  "\C-hf"           'sql-tables
  "\C-hF"			'sql-find-column
  "\C-hv"           'sql-describe
  "\C-hs"           'sql-show-table
  "\C-ho"           'sql-inspect-table
  )


;; -------------------------------------------------------------------------- ;;
;; Functions                                                                  ;;
;; -------------------------------------------------------------------------- ;;

;; ---------- Capitalization ----------
(defun sqlup-capitalize-keywords ()
  "Capitalize SQL keywords in buffer or region.

See `sqlup-capitalize-keywords-in-region' for more info."
  (interactive)
  (with-region-or-buffer #'sqlup-capitalize-keywords-in-region))

(defun sqlup-capitalize-keywords-in-line ()
  "Capitalize SQL keywords in line or region.

See `sqlup-capitalize-keywords-in-region' for more info."
  (interactive)
  (with-region-or-line #'sqlup-capitalize-keywords-in-region))

(advice-add 'sqlind-indent-line :after #'sqlup-capitalize-keywords-in-line)

;; ---------- Indentation ----------
(defun sqlind-syntax-of-line-recursive ()
  (let* ((syntax-of-line (sqlind-syntax-of-line)))
    (if (memq (sqlind-syntax-symbol syntax-of-line)
              '(nested-statement-open
                nested-statement-continuation
                nested-statement-close))
        (save-excursion
          (goto-char (sqlind-anchor-point syntax-of-line))
          (cons (car syntax-of-line) (sqlind-syntax-of-line-recursive)))
      syntax-of-line)))

(defun sqlind-indent-directive (syntax base-indentation)
  "Indents lines starting with a directive.

See `sqlind-ms-directive', `sqlind-sqlite-directive',
`sqlind-sqlplus-directive' for regexp.
    "
  (save-excursion
    (back-to-indentation)
    (let ((rx (cl-case (and (boundp 'sql-product) sql-product)
                (ms sqlind-ms-directive)
                (sqlite sqlind-sqlite-directive)
                (oracle sqlind-sqlplus-directive)
                (t nil))))
      (if (looking-at rx)
          (+ base-indentation sqlind-basic-offset)
        base-indentation))))

(defun sqlind-indent-logic (syntax base-indentation)
  "Indents lines starting with AND, OR, NOT"
  (save-excursion
    (back-to-indentation)
    (if (looking-at "and\\|or\\|not")
        (+ base-indentation sqlind-basic-offset)
      base-indentation)))

(defun sqlind-indent-comment-start-previous-line (syntax base-indentation)
  "Return previous line's comment column if exist.

If we start a line comment (--) and the previous line also has a
line comment, we line up the two comments.  Otherwise return
BASE-INDENTATION."
  (save-excursion
    (back-to-indentation)
    (when (and (looking-at "\\s *--")
	           (progn
	             (forward-line -1)
	             (re-search-forward "--" (line-end-position) t)))
	  (goto-char (match-beginning 0))
	  (setq base-indentation (current-column)))
    base-indentation))

(defun sqlind-indent-comment-start-transparent (syntax base-indentation)
  "Returns indentation of line as if it were not commented.
"
  (save-excursion
    (back-to-indentation)    
    (let ((sqlind-comment-start-skip "\`"))
        (sqlind-calculate-indentation (sqlind-syntax-of-line)))))

(defun sqlind-indent-select-join (syntax base-indentation)
  "Indent lines beginning with JOIN"
  (save-excursion
    (back-to-indentation)
    (if (or (looking-at sqlind-select-join-regexp)
            (looking-at "\\bjoin\\b"))
        (+ base-indentation sqlind-basic-offset)
      base-indentation)))

(defun sqlind-set-indentation-default ()
  "Reset `sqlind' to `sqlind-default-indentation-offsets-alist'"
  (interactive)
  (setq
   sqlind-indentation-offsets-alist
   sqlind-default-indentation-offsets-alist))

(defun sqlind-set-indentation ()
  (interactive)
  (setq
   sqlind-indentation-offsets-alist
   `((syntax-error                  sqlind-report-sytax-error)
     
     ;; top most indent level
     (toplevel                      sqlind-indent-directive)

     ;; ------------- COMMENTS -------------
     ;; -- comment     <- comment-start
     ;; /*             <- comment-start
     ;;   comment      <- comment-continuation
     ;;  */            <- comment-continuation
     (comment-start                 sqlind-indent-comment-start-transparent
                                    sqlind-indent-comment-start-previous-line)
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
     ;;   T3               <- select-table
     ;;       ON           <- select-join-condition
     ;;                        ^ ANCHOR - JOIN statement
     ;;     c3 = c2        <- select-table-continuation
     ;;                        ^ ANCHOR - table definition
     ;;       AND          <- select-join-condition
     ;;     d3 = d1        <- select-table-continuation
     (select-table                  sqlind-indent-select-table
                                    sqlind-indent-select-join)
     (select-table-continuation     +
                                    sqlind-lone-semicolon)
     (select-join-condition         sqlind-indent-logic)      
     
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
                                    sqlind-indent-logic
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


;; ---------- Connection Completion ----------
(defun sql-read-connection-filtered (prompt &optional initial default)
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

(defun sql-connection-annotate (cand)
  "Annotate SQL connections with the server name."
  (when-let (sym (cdr (assoc-string cand sql-connection-alist t)))
    
    (let ((server
           (-last-item
            (--first (eq (car it) 'sql-server) sym))))
      (marginalia--fields
       (server :truncate 1.0 :face 'marginalia-documentation)))))

(add-to-list 'marginalia-command-categories
              '(sql-connect . sql-connection))
;; (add-to-list 'marginalia-command-categories
;;              '(sql-set-repl . sql-connection))
(add-to-list 'marginalia-annotator-registry
              '(sql-connection sql-connection-annotate builtin none))

(advice-add 'sql-read-connection :override #'sql-read-connection-filtered)


;; ---------- Product Completion ----------
(defun sql-read-product-no-init (prompt &optional initial)
  "Read a valid SQL product without an initial value already selected"
  (let ((init (or (and initial (symbol-name initial)) "ansi")))
    (intern (completing-read
             prompt
             (mapcar (lambda (info) (symbol-name (car info)))
                     sql-product-alist)
             nil 'require-match
             nil 'sql-product-history init))))

(advice-add 'sql-read-product :override #'sql-read-product-no-init)


;; ---------- REPL ----------
(defun sql-create-repl ()
  "Connect to an interactive session using CONNECTION settings.

See `sql-connection-alist' to see how to define connections and
their settings.

The user will not be prompted for any login parameters if a value
is specified in the connection settings."

  (interactive)
  ;; Get connection settings
  (let* ((connection (sql-read-connection "Connection: "))
         (calling-buffer (current-buffer))
         (sqli-buffer (sql-generate-buffer-name connection))
         (connect-set
          (cdr (assoc-string connection sql-connection-alist t)))
         param-var login-params set-vars rem-vars)    
    
    (setq sql-connection connection)

    ;; Set the parameters and start the interactive session
    (dolist (vv connect-set)
      (let ((var (car vv))
            (val (cadr vv)))
        (set-default var (eval val))))

    ;; :sqli-login params variable
    (setq param-var
          (sql-get-product-feature sql-product :sqli-login nil t))

    ;; :sqli-login params value
    (setq login-params (symbol-value param-var))

    ;; Params set in the connection
    (setq set-vars
          (mapcar
           (lambda (v)
             (pcase (car v)
               ('sql-user     'user)
               ('sql-password 'password)
               ('sql-server   'server)
               ('sql-database 'database)
               ('sql-port     'port)
               (s             s)))
           connect-set))
    
    ;; the remaining params (w/o the connection params)
    (setq rem-vars
          (sql-for-each-login
           login-params
           (lambda (var vals)
             (unless (member var set-vars)
               (if vals (cons var vals) var)))))

    ;; Start the SQLi session with revised list of login parameters
    (eval `(let ((,param-var ',rem-vars))
             (sql-product-interactive ',sql-product ',sqli-buffer)))    
    (setq repl-buffer       calling-buffer
          sql-connection    connection)
    
    (pop-to-buffer calling-buffer)
    (sql-eval-init)
    sql-buffer))

(defun sql-generate-buffer-name (connection)
  "Generate a new, unique buffer name for a SQLi buffer.

Append a sequence number until a unique name is found."
  (let ((buf-name  (format "*SQL: <%s>*" connection))
        (buf-fmt-rest (format "*SQL: <%s> <%%d>*" connection))
        (i 2))
    
    ;; See if we can find an unused buffer
    (while
        (or
         (and (sql-is-sqli-buffer-p buf-name)
              (comint-check-proc buf-name))
         (buffer-live-p (get-buffer buf-name)))
      
      ;; Check a sequence number on the BASE
      (setq buf-name (format buf-fmt-rest i)
            i (1+ i)))

    buf-name))

(defun sql-set-repl ()
  (when-let* ((this-product sql-product)
              (default-buffer (sql-find-sqli-buffer this-product))
              (buffer-count
               (length (--filter
                        (with-current-buffer it
                          ;; AND (eq sql-product this-product)
                          (eq major-mode 'sql-interactive-mode))
                        (buffer-list))))
              (new-buffer
               (if (= buffer-count 1)
                   default-buffer
                 (read-buffer "New SQLi buffer to use: " default-buffer t
                              (lambda (it)
                                (with-current-buffer (car it)
                                  (eq major-mode 'sql-interactive-mode)))))))
    (if (null (sql-buffer-live-p new-buffer))
        (user-error "Buffer %s is not a working SQLi buffer" new-buffer)
      (when new-buffer
        (setq sql-buffer new-buffer)
        (run-hooks 'sql-set-sqli-hook)))
    (setq sql-connection
          (buffer-local-value 'sql-connection (get-buffer sql-buffer)))
    sql-buffer))

(defun sql-eval ()
  "Evaluates SQL code."
  (interactive)

  (with-region-or-paragraph #'sql-send-region)
  (unless (use-region-p)
    (next-paragraph)))

(defun sql-eval-file (file)
  "Evaluates an SQL file."
  (interactive
   (read-file-name "SQL File: " nil nil t nil
                   (lambda (it) (or (f-ext-p it "sql")
                               (f-dir-p it)))))
  (sql-send-string (f-read file 'utf-8)))



;; -----------------------------------------------------------------------------
;; Help Functions
;; -----------------------------------------------------------------------------
(defun sql-table-at-point ()
  (save-excursion
    (let*
        ((table-start
          (progn
            (search-backward-regexp "[^._[:alnum:]]" (point-min) 1)
            (unless (bobp)
              (forward-char))
            (point)))
	     (table-end
          (progn
		    (search-forward-regexp "[^._[:alnum:]]" (point-max) 1)
            (unless (eobp)
              (backward-char))
            (point))))
      (buffer-substring-no-properties table-start table-end))))

(defvar sql-prompt-history-table nil
  "History list of tables used for commands")
(defvar sql-prompt-history-schema nil
  "History list of schema used for commands")

(defun sql-prompt-read-string (prompt &optional history default)
  "Prompt for string and wrap with wildcard, %.

By default, the string is assumed to be wrapped with wildcard
characters to facilitate WHERE ... LIKE \"%STRING%\".  On the
other hand if the string begins or ends with %, negate them."
  (let*
      ((prompt-input
        (read-string prompt nil history default))
       (prompt-input
        (if (s-blank-p prompt-input)
            "%%"
          (s-with prompt-input s-upcase
                  (s-prepend "%")
                  (s-chop-prefix "%%")
                  (s-append "%")
                  (s-chop-suffix "%%")))))
    prompt-input
  ))

(defun sql-eval-product-query (query)
  "Execute QUERY from `sql-product-alist'.

QUERY is a SQL product feature beginning with \":query-\".  Prior
to execution, parameter strings are substituted from the current
context using `s-lex-format', such that \"${var}\" will be replaced
by the variable \"var\". The following is a list of variable in use:
    - ${schema-table} - SCHEMA.TABLE string see `sql-table-at-point'
    - ${schema} - just the SCHEMA
    - ${table} - just the TABLE
    - ${column}
    - ${user} - for reconnecting and searching
    - ${password}
    - ${database}

This is the general function for subsequent help commands."
  (if-let*
      ((query-name
        (s-with (symbol-name query)
          (s-replace ":query-" "")  s-upcase))
       (product-name (s-upcase (symbol-name sql-product)))
       (query-string (sql-get-product-feature
                      sql-product query))
       (query-string (s-trim query-string))
       (query-string (if (s-ends-with? ";" query-string)
                         query-string
                       (s-append ";" query-string))))
      
      (progn
        (sql-send-string (eval (s-lex-fmt|expand query-string)))
        (repl-buffer-show))
    
	(message-format
     "${query-name} not defined for product ${product-name}")))


;; ---------- Commands ---------- ;;
(defun sql-describe (schema-table)
  "Describe the current table"
  (interactive
   (list (sql-table-at-point)))
  (if (stringp schema-table)
      (sql-eval-product-query :query-describe-table)
    (message "No SCHEMA-TABLE given.")))

(defun sql-show-table (schema-table)
  "Lists the top elements of a table"
  (interactive
   (list (sql-table-at-point)))
  (if (stringp schema-table)
      (sql-eval-product-query :query-show-table)
    (message "No SCHEMA-TABLE given.")))

(defun sql-list-grantees (table)
  "Lists grantee privileges of a user table."
  (interactive
   (list (sql-table-at-point)))  
  (if (not (stringp schema-table))
      (message "No TABLE given.")
    (setq table (-last-item (s-split "\\." table)))
    (sql-eval-product-query :query-list-grantees)))

(defun sql-eval-init ()
  "Evaluates SQL Init File."
  (interactive)
  (sql-eval-product-query :query-init))

(defun sql-tables (schema table)
  "Lists tables (or views) that match table / schema pattern"
  (interactive
   (list
    (sql-prompt-read-string "Schema: "  'sql-prompt-history-schema)
    (sql-prompt-read-string "Table: "   'sql-prompt-history-table)))
  (sql-eval-product-query :query-find-table))

(defun sql-find-column (column schema table)
  "Find all tables / views that contain column"
  (interactive
   (list
    (sql-prompt-read-string "Column: ")
    (sql-prompt-read-string "Schema: "  'sql-prompt-history-schema)
    (sql-prompt-read-string "Table: "   'sql-prompt-history-table)))
  (sql-eval-product-query :query-find-column))

(defun sql-find-user (user)
  "Find all matching users"
  (interactive
   (list (sql-prompt-read-string "User: ")))
  (sql-eval-product-query :query-find-user))

(defun sql-reconnect ()
  "Reconnects to database using the associated sqli buffer"
  (interactive)
  (-let* ((connect-set
           (cdr (assoc-string sql-connection sql-connection-alist t)))
          ((&alist 'sql-user (user) 'sql-password (password)
                    'sql-server (server) 'sql-database (database))
           connect-set))
    (sql-eval-product-query :query-connect)
    (sql-eval-init)))


;; -------------------------------------------------------------------------- ;;
;; Syntax Highlighting                                                        ;;
;; -------------------------------------------------------------------------- ;;
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



(defun sql-send-statement ()
  (interactive)
  (let* ((statement-start
		  (save-excursion (sql-beginning-of-statement 0) (point)))
         (statement-end
		  (save-excursion (sql-end-of-statement 0) (point))))
    (message "%s %s" statement-start statement-end)))


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








;; sql-prompt-regexp

;; (let ((comint-preoutput-filter-functions
;; 	  '(sql-interactive-remove-continuation-prompt)))
;;   (sql-redirect
;;    (sql-find-sqli-buffer)
;;    "
;; describe ADL_DEV3.NCRE_RULES;"
;;    " *SQL Echo Area*"))


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





;; (defun sql-user-tables (pattern)
;;   "Show all USER tables"
;;   (interactive "sPattern: ")
;;   (let ((func (sql-get-product-feature sql-product :func-user-tables)))
;;     (if func     
;; 	    (progn ; FIXME was save-frame-excursion 
;; 	      (sql-send-string
;; 	       (replace-regexp-in-string
;; 	        "PATTERN"
;; 	        (upcase pattern)
;; 	        func))
;; 	      (display-buffer sql-buffer))
;; 	  (message "no :func-user-tables defined for product %s" sql-product))))

;; (defun sql-user-functions ()
;;   "Shows all USER functions"
;;   (interactive)
;;   (let ((func (sql-get-product-feature sql-product :func-user-functions)))
;;     (if func     
;; 	    (progn ; FIXME was save-frame-excursion 
;; 	      (sql-send-string func)
;; 	      (display-buffer sql-buffer))
;; 	  (message "no :func-user-functions defined for product %s" sql-product))))

;; (defun sql-last-error ()
;;   "Shows last error"
;;   (interactive)
;;   (let ((func (sql-get-product-feature sql-product :func-last-error)))
;;     (if func     
;; 	    (progn ; FIXME was save-frame-excursion 
;; 	      (sql-send-string func)
;; 	      (display-buffer sql-buffer))
;; 	  (message "no :func-last-error defined for product %s" sql-product))))

;; (defun sql-explain ()
;;   "Explain plan of code"
;;   (interactive)
;;   (let ((func (sql-get-product-feature sql-product :func-explain)))
;;     (if func
;; 	    (progn ; FIXME was save-frame-excursion  
;; 	      (sql-send-string (replace-regexp-in-string
;; 					        "REGION"
;; 					        (get-region-as-string)
;; 					        func)))
;; 	  (message "no :func-explain defined for product %s" sql-product))))


;; (defun sql-describe-here ()
;;   (interactive)
;;   (sql-output-here
;;    '(lambda ()
;; 	  (sql-describe)
;; 	  (end-of-line)
;; 	  (newline))))

;; (defun sql-inspect-table ()
;;   "Lists some table info"
;;   (interactive)
;;   (let ((func (sql-get-product-feature sql-product :func-inspect-table)))
;;     (if func     
;; 	    (sql-send-string (replace-regexp-in-string
;; 					      "TABLE_PATTERN"
;; 					      (upcase (sql-table-at-point))
;; 					      func))
;; 	  (message "no :func-inspect-table defined for product %s" sql-product))))

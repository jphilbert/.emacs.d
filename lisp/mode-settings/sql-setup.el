;; ----------------------------------------------------------------------------
;; SQL MODE
;; ----------------------------------------------------------------------------
(provide 'sql-setup)

;; SQL Servers (put in secure location)
(require 'sql-servers "~/OneDrive - UPMC/sql-servers.el" t) 
(require 'sql-indent) 
(add-to-list 'ac-modes 'sql-mode)

;; (setq sql-ms-program		"C:/Program Files/Microsoft SQL Server/100/Tools/Binn/sqlcmd.exe"
;;       sql-oracle-program		"sqlplus"
;; 	 sql-oracle-scan-on		nil
;; 	 sql-send-terminator	nil		; since I don't put GO after
;; 								; (CAUSE ISSUES IN SQLPLUS if non-nil)
;; 	 sql-ms-options		'("-w" "2000" ; Max Column Width
;; 						  "-y" "2000" ; Individual Char Width
;; 						  "-s" "|"    ; Column Separator
;; 						  "-k")
;; 	 )

;; Adds _ as a word characters so abbrev-mode doesn't changes parts of strings
(modify-syntax-entry ?_ "w" sql-mode-syntax-table)

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(defun my-sql-mode-hook ()
  (interactive)
 
  (hs-minor-mode t)
  (setq ac-sources
  	   (append '(ac-source-yasnippet) ac-sources))
  (flyspell-prog-mode)
  (turn-on-auto-fill)  
  (rainbow-delimiters-mode 1)

  (add-function :around (local 'abbrev-expand-function)
    #'sql-mode-abbrev-expand-function)  
  (abbrev-mode t)

  (make-local-variable 'sql-product)

  (sql-set-product 'oracle)
  )

(defun my-sql-interactive-mode-hook ()
  (interactive)
  (text-scale-set -1.1)
  
  (setq comint-preoutput-filter-functions nil)
  ;; (add-hook 'comint-preoutput-filter-functions
  ;;           'sql-interactive-remove-continuation-prompt nil t)
  (add-hook 'comint-preoutput-filter-functions
  		  'sql-add-newline-first t t)

  (sql-rename-buffer)

  (auto-complete-mode t)
  (abbrev-mode t)
  ;; (setq ac-ignore-case nil)
  ;; (setq-default truncate-lines t)
  )


;; --------------------------------------------------------------------------
;; Key Binding
;; --------------------------------------------------------------------------
(define-many-keys sql-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]  'sql-eval
  [(M-return)]		'sql-eval-here

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")	'tab-to-tab-stop-magic
  (kbd "<tab>")	'sql-fix-indent

  ;; ---------- Frame Switching ----------
  [(f12)]			'sql-switch-frame-process
  [S-f12]           'sql-connect
  [C-f12]			'sql-set-sqli-buffer
  [M-f12]           'sql-set-product


  ;; ---------- Help ----------
  [(f1)]		  	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  [(S-f1)]	  	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
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
  [S-C-up]		'previous-line
  [S-C-down]		'next-line

  ;; ---------- Frame Switching ----------
  [(f12)]           'sql-switch-frame-script
  [S-f12]           'sql-reconnect


  ;; ---------- Completion ----------
  (kbd "<tab>")	'completion-at-point

  ;; ---------- Help ----------
  [(f1)]		  	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  [(S-f1)]	  	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
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

;; Describe 
(sql-set-product-feature
 'postgres
 :func-desc "\\d+ PATTERN;")

(sql-set-product-feature
 'oracle
 :func-desc "
describe PATTERN;")

(sql-set-product-feature
 'ms
 :func-desc "select left(COLUMN_NAME, 40) as COL_NAME,
left(DATA_TYPE, 20) as DATE_TYPE, CAST(CHARACTER_MAXIMUM_LENGTH AS varchar(6)) as length, IS_NULLABLE
from INFORMATION_SCHEMA.COLUMNS where TABLE_schema + '.' +  TABLE_NAME='PATTERN'
GO
")

;; Show Table
(sql-set-product-feature
 'postgres
 :func-show-table "select * from TABLE limit 5;")

(sql-set-product-feature
 'oracle
 :func-show-table "
select * from TABLE where rownum <= 5;")

(sql-set-product-feature
 'ms
 :func-show-table "select top (5) * from TABLE
GO")

;; Inspect Table
(sql-set-product-feature
 'oracle
 :func-inspect-table  "
     SELECT
        round(sum(bytes)/(1024*1024),1) AS size_mb,
	sum(NUM_ROWS) as number_rows,
	max(created) as created,
	cast(COMMENTS as varchar2(20)) as comments
      FROM
	user_extents
	JOIN
	all_tables
	ON segment_name = table_name
	join
	all_objects
	on table_name = object_name
	left join
	user_tab_comments using(table_name)
      WHERE
        segment_type = 'TABLE'
	   and
	   table_name = 'TABLE_PATTERN'
      GROUP BY
        table_name,
	COMMENTS
      order by
        table_name;
     ")

;; Initial File
(sql-set-product-feature
 'oracle
 :init-file "~/OneDrive - UPMC/sql_oracle_init.sql")

(sql-set-product-feature
 'ms
 :init-file "~/OneDrive - UPMC/sql_ms_init.sql")


;; List Tables
(sql-set-product-feature
 'oracle
 :func-list-tables "
     SELECT
		type, substr(owner || '.' || name, 1, 70) AS name
	FROM
	(
		SELECT 'V' as type, owner, view_name as name
		FROM all_views
		UNION
		SELECT 'T' as type, owner, table_name as name
		FROM all_tables)
	WHERE owner LIKE '%OWNER_PATTERN%'
        AND name LIKE '%TABLE_PATTERN%'
     ORDER BY type, owner, name;")

(sql-set-product-feature
 'ms
 :func-list-tables "SELECT top 100
    concat(left(TABLE_SCHEMA, 20), '.',
      left(TABLE_NAME, 45)) TABLE_NAME,
    left(TABLE_TYPE, 10) TABLE_TYPE
FROM
    INFORMATION_SCHEMA.TABLES
WHERE
    TABLE_SCHEMA LIKE '%OWNER_PATTERN%'
    AND
    TABLE_NAME LIKE '%TABLE_PATTERN%'
ORDER BY 1
go")



;; Find Column
(sql-set-product-feature
 'oracle
 :func-find-column "SELECT
	   substr(owner || '.' || table_name, 1, 48) AS table_name,
	   substr(column_name, 1, 28)
    from
		all_tab_columns
    where
		column_name like '%PATTERN%'
    order by
		owner, table_name, column_name;")

(sql-set-product-feature
 'ms
 :func-find-column "select top 100
	    concat(left(TABLE_SCHEMA, 20), '.',
             left(TABLE_NAME, 45)) TABLE_NAME,
		left(COLUMN_NAME, 30) COLUMN_NAME
	FROM
		INFORMATION_SCHEMA.COLUMNS
	WHERE
		column_name like '%PATTERN%'
	ORDER BY
		TABLE_SCHEMA, table_name, column_name
go")

;; Show User Tables
(sql-set-product-feature
 'oracle
 :func-user-tables  "SELECT
        table_name,
        round(sum(bytes)/(1024*1024),1) AS size_mb,
	sum(NUM_ROWS) as number_rows,
	max(created) as created,
	cast(COMMENTS as varchar2(20)) as comments
      FROM
	user_extents
	JOIN
	all_tables
	ON segment_name = table_name
	join
	all_objects
	on table_name = object_name
	left join
	user_tab_comments using(table_name)
      WHERE
        segment_type = 'TABLE'
	and
	table_name like '%PATTERN%'
      GROUP BY
        table_name,
	COMMENTS
      order by
        table_name;
     SELECT
       tablespace_name,
       bytes / 1024 / 1024 as used_in_mb,
       -- max_bytes / 1024 / 1024 as max_in_mb,
	cast(trunc(100 * bytes / max_bytes) as varchar2(3)) ||
	' %' as used
     FROM
       USER_TS_QUOTAS
     WHERE
	max_bytes > 0;")

;; Show User Functions
(sql-set-product-feature
 'oracle
 :func-user-functions "SELECT
	object_name,
	object_type,
	status
     FROM
	ALL_OBJECTS
     WHERE
	OBJECT_TYPE
	IN ('FUNCTION','PROCEDURE') and
	owner = user;")

;; Last Error
(sql-set-product-feature
 'oracle
 :func-last-error "set underline off;
	select *
	from SYS.USER_ERRORS
	WHERE rownum = 1
	ORDER BY rownum DESC;
	set underline on;")

;; Explain
(sql-set-product-feature
 'oracle
 :func-explain  "explain plan for (REGION);
	select plan_table_output
	from table(dbms_xplan.display('plan_table',null,'basic +cost'))
	union all
	select plan_table_output
	from table(dbms_xplan.display('plan_table',null,'basic +bytes +rows'))
	union all
	select plan_table_output from
	table(dbms_xplan.display('plan_table',null,'typical -cost -bytes -rows
-partition -parallel +PREDICATE +note'));")

;; Connect
(sql-set-product-feature
 'oracle
 :func-con "CONNECT %1s/%2s@%3s;")





;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
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
(advice-add 'sql-read-connection
		  :around #'sql-read-connection-filtered)

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
(advice-add 'sql-send-string :around
		  #'(lambda (orig-fun &rest args)
			 (save-frame-excursion (apply orig-fun args))))


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

(defun sql-output-here (func)
  (interactive)
  (cl-letf (((symbol-function 'sql-send-string)
		   #'(lambda (str)
  			 (sql-redirect
  			  (sql-find-sqli-buffer)
  			  str
  			  " *SQL Echo Area*"))))
    ;; (advice-add 'sql-send-string :around
    ;; 			 #'(lambda (orig-fun &rest args)
    ;; 				(sql-redirect
    ;; 				 (sql-find-sqli-buffer)
    ;; 				 args
    ;; 				 " *SQL Echo Area*")))
    ;; (let ((comint-preoutput-filter-functions
    ;; 		 '(sql-interactive-remove-continuation-prompt)))
	 (funcall func)
	 ;; )
    ;; (advice-remove 'sql-send-string 
    ;; 			    #'(lambda (orig-fun &rest args)
    ;; 				   (sql-redirect
    ;; 				    (sql-find-sqli-buffer)
    ;; 				    args			  
    ;; 				    " *SQL Echo Area*")))
    ))

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

(defun sql-eval-here ()
  (interactive)
  (sql-output-here
   '(lambda ()
	 (sql-eval)
	 ;; since the eval auto increments to the next line
	 (previous-non-blank-line)
	 (end-of-line)
	 (newline))))

(defun sql-eval ()
  "Evaluates SQL code."
  (interactive)
  ;; Pre Eval
  (unless sql-buffer
    (if (null (sql-find-sqli-buffer sql-product))
	   ;; Connect
	   (call-interactively 'sql-connect)
	 ;; else Set
	 (sql-set-sqli-buffer)))

  (unless (and transient-mark-mode mark-active)
    (mark-paragraph))
  (sql-send-region (region-beginning) (region-end))
  (deactivate-mark)
  (forward-paragraph)
  (next-non-blank-line))

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

(defun sql-fix-indent ()
  "Fixes indents for a whole paragraph. Pretty much all one should need."
  (interactive)
  (save-excursion
    (progn
      (mark-paragraph)
      (call-interactively 'indent-region))))


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
  (save-frame-excursion
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
  (let ((func (sql-get-product-feature sql-product :func-desc)))
    (if func     
	   (sql-send-string (replace-regexp-in-string
					 "PATTERN"
					 (upcase (sql-table-at-point))
					 func))
	 (message "no :func-desc defined for product %s" sql-product))))

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
	 (save-frame-excursion
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
	 (save-frame-excursion
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
	 (save-frame-excursion
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
	 (save-frame-excursion
	  (sql-send-string func)
	  (display-buffer sql-buffer))
	 (message "no :func-user-functions defined for product %s" sql-product))))

(defun sql-last-error ()
  "Shows last error"
  (interactive)
   (let ((func (sql-get-product-feature sql-product :func-last-error)))
    (if func     
	 (save-frame-excursion
	  (sql-send-string func)
	  (display-buffer sql-buffer))
	 (message "no :func-last-error defined for product %s" sql-product))))

(defun sql-explain ()
  "Explain plan of code"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-explain)))
    (if func
	   (save-frame-excursion 
	    (sql-send-string (replace-regexp-in-string
					  "REGION"
					  (get-region-as-string)
					  func)))
	 (message "no :func-explain defined for product %s" sql-product))))


;; Suppress Abbrev in Comments 
(defun sql-mode-abbrev-expand-function (expand)
  (if (or
	  (nth 3 (syntax-ppss))		; inside string.
	  (nth 4 (syntax-ppss))		; inside comment
	  )
	 ;; Use the text-mode abbrevs.
	 (let ((local-abbrev-table text-mode-abbrev-table))
	   (funcall expand))
    ;; Else performs normal expansion.
    (funcall expand)
    )
  )

;; Fix SQL Highlighting + Rainbow Delimiters
;;	sql-highlight-product overrides rainbow-delimiters so we need to reapply it
;;	afterwords if it was on
(advice-add 'sql-highlight-product :around
		  '(lambda (func)
			(let ((rainbow-state rainbow-delimiters-mode))
			  (funcall func)
			  (rainbow-delimiters-mode rainbow-state))))

;; --------------------------------------------------------------------------
;; Additional Keywords
;; --------------------------------------------------------------------------
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

(define-abbrev-table 'sql-mode-abbrev-table
  (mapcar #'(lambda (v) (list v (upcase v) nil 1))
		'("absolute" "action" "add" "after" "all" "allocate" "alter" "and"
  "any" "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at"
  "atomic" "authorization" "avg" "before" "begin" "between" "bigint" "binary"
  "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call" "called"
  "cascade" "cascaded" "case" "cast" "catalog" "char" "char_length" "character"
  "character_length" "check" "clob" "close" "coalesce" "collate" "collation"
  "column" "commit" "condition" "connect" "connection" "constraint"
  "constraints" "constructor" "contains" "continue" "convert" "corresponding"
  "count" "create" "cross" "cube" "current" "current_date"
  "current_default_transform_group" "current_path" "current_role" "current_time"
  "current_timestamp" "current_transform_group_for_type" "current_user" "cursor"
  "cycle" "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default"
  "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe"
  "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do"
  "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif" "end"
  "equals" "escape" "except" "exception" "exec" "execute" "exists" "exit"
  "external" "extract" "false" "fetch" "filter" "first" "float" "for" "foreign"
  "found" "free" "from" "full" "function" "general" "get" "global" "go" "goto"
  "grant" "group" "grouping" "handler" "having" "hold" "hour" "identity" "if"
  "immediate" "in" "indicator" "initially" "inner" "inout" "input" "insensitive"
  "insert" "int" "integer" "intersect" "interval" "into" "is" "isolation"
  "iterate" "join" "key" "language" "large" "last" "lateral" "leading" "leave"
  "left" "level" "like" "local" "localtime" "localtimestamp" "locator" "loop"
  "lower" "map" "match" "map" "member" "merge" "method" "min" "max" "minute"
  "modifies" "module" "month" "multiset" "names" "national" "natural" "nchar"
  "nclob" "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object"
  "octet_length" "of" "old" "on" "only" "open" "option" "or" "order"
  "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter"
  "partial" "partition" "path" "position" "precision" "prepare" "preserve"
  "primary" "prior" "privileges" "procedure" "public" "range" "read" "reads"
  "real" "recursive" "ref" "references" "referencing" "relative" "release"
  "repeat" "resignal" "restrict" "result" "return" "returns" "revoke" "right"
  "role" "rollback" "rollup" "routine" "row" "rows" "savepoint" "schema" "scope"
  "scroll" "search" "second" "section" "select" "sensitive" "session"
  "session_user" "set" "sets" "signal" "similar" "size" "smallint" "some"
  "space" "specific" "specifictype" "sql" "sqlcode" "sqlerror" "sqlexception"
  "sqlstate" "sqlwarning" "start" "state" "static" "submultiset" "substring"
  "sum" "symmetric" "system" "system_user" "table" "tablesample" "temporary"
  "then" "time" "timestamp" "timezone_hour" "timezone_minute" "to" "trailing"
  "transaction" "translate" "translation" "treat" "trigger" "trim" "true"
  "under" "undo" "union" "unique" "unknown" "unnest" "until" "update" "upper"
  "usage" "user" "using" "value" "values" "varchar" "varying" "view" "when"
  "whenever" "where" "while" "window" "with" "within" "without" "work" "write"
  "year" "zone")))




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


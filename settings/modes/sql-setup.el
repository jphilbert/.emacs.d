;; ----------------------------------------------------------------------------
;; SQL MODE
;; ----------------------------------------------------------------------------
(provide 'sql-setup)
(require 'sql)


;; SQL Servers (put in secure location)
(require 'sql-servers "~/OneDrive - UPMC/sql-servers.el" t) 


(eval-after-load "sql" '(load-library "sql-indent")) 
(add-to-list 'ac-modes 'sql-mode)

(setq sql-ms-program		"C:/Program Files/Microsoft SQL Server/100/Tools/Binn/sqlcmd"
      sql-oracle-program		"sqlplus"
	 sql-oracle-scan-on		nil
	 sql-send-terminator	nil		; since I don't put GO after
								; (CAUSE ISSUES IN SQLPLUS if non-nil)
	 ;; sql-ms-options		'("-w" "80")
	 sql-ms-options		'("-w" "80" "-y" "79" "-s" "|" "-k")
	 )

(sql-set-product-feature
 'ms :prompt-regexp "^[0-9]*>") ;existing line
(sql-set-product-feature
 'ms :prompt-cont-regexp "^[0-9]*>") ;new line


;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook ()
  (interactive)
  (require 'ac-sql)
  (add-to-list 'ac-sources 'ac-source-sql)
  
  (hs-minor-mode t)
  ;; (hs-hide-all)
  
  (flyspell-prog-mode)
  (turn-on-auto-fill)

  (add-function :around (local 'abbrev-expand-function)
			 #'sql-mode-abbrev-expand-function)
  
  (abbrev-mode t)
  
  ;; (sql-set-product 'oracle)

  ;; (setq comment-start "/*") **/
  ;; (setq comment-end "*\/") **/
  )

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(defun my-sql-interactive-mode-hook ()
  (text-scale-set -1.1)
  (sql-rename-buffer)
  
  (add-to-list 'ac-sources 'ac-source-sql)
  (auto-complete-mode t)

  (abbrev-mode t)
  ;; (setq ac-ignore-case nil)

  ;; (setq-default truncate-lines t)
  )

;; --------------------------------------------------------------------------
;; Keybinding
;; --------------------------------------------------------------------------
(define-many-keys sql-interactive-mode-map
  ;; ---------- Input / Prompt Scrolling ----------
  [C-up]               'comint-previous-prompt
  [C-down]             'comint-next-prompt
  [up]                 'comint-previous-input
  [down]               'comint-next-input
  [S-C-up]			'previous-line
  [S-C-down]			'next-line
  

  ;; ---------- Completion ----------
  (kbd "<tab>")	'completion-at-point

  ;; ---------- Help ----------
  [(S-f1)]	  	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
  "\C-hf"              'sql-tables
  ;; "\C-he"              'sql-explain
  "\C-hv"              'sql-describe
  "\C-hs"              'sql-show-table
  "\C-ho"              'sql-inspect-table

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-next-sql
  [S-f12]              'sql-connect
  )

(define-many-keys sql-mode-map
  ;; ---------- Evaluation ----------
  [(shift return)]     'sql-eval

  ;; ---------- Indent / Tabs ----------
  (kbd "<C-tab>")		'tab-to-tab-stop-magic
  (kbd "<tab>")		'sql-fix-indent

  ;; ---------- Frame Switching ----------
  [(f12)]              'switch-frame-current-sql
  [S-f12]              'sql-connect
  [C-f12]              'sql-set-sqli-buffer

  ;; ---------- Help ----------
  [(S-f1)]	   	'(lambda ()
				   (interactive)
				   (google-query-at-point t (format "SQL %s "
											 sql-product)))
  (kbd "C-h w")   	'(lambda ()
				   (interactive)
				   (google-query-at-point nil (format "SQL %s "
											   sql-product)))
  "\C-hf"              'sql-tables
  ;; "\C-he"              'sql-explain
  "\C-hv"              'sql-describe
  "\C-hs"              'sql-show-table
  "\C-ho"              'sql-inspect-table
  )

;; --------------------------------------------------------------------------
;; Product Features
;; --------------------------------------------------------------------------
;; Describe 
(sql-set-product-feature
 'postgres
 :func-desc "\\d+ PATTERN;")

(sql-set-product-feature
 'oracle
 :func-desc "describe PATTERN;")

(sql-set-product-feature
 'ms
 :func-desc "select left(COLUMN_NAME, 40) as COL_NAME, IS_NULLABLE, left(DATA_TYPE, 20) as DATE_TYPE from INFORMATION_SCHEMA.COLUMNS where TABLE_schema + '.' +  TABLE_NAME='PATTERN'
GO
")

;; Show Table
(sql-set-product-feature
 'postgres
 :func-show-table "select * from TABLE limit 5;")

(sql-set-product-feature
 'oracle
 :func-show-table "select * from TABLE where rownum <= 5;")

(sql-set-product-feature
 'ms
 :func-show-table "select top (5) * from TABLE
GO")

;; Inspect Table
(sql-set-product-feature
 'oracle
 :func-inspect-table  "SELECT
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
 :init-file "~/OneDrive - UPMC/sql defaults.sql")

;; List Tables
(sql-set-product-feature
 'oracle
 :func-list-tables "SELECT * FROM
	(SELECT 'V' as type, owner, view_name as name
	FROM all_views
	UNION
	SELECT 'T' as type, owner, table_name as name
	FROM all_tables
        ORDER BY owner, name)
	WHERE owner LIKE '%OWNER_PATTERN%'
        AND name LIKE '%TABLE_PATTERN%';")

(sql-set-product-feature
 'ms
 :func-list-tables "SELECT top 100
    left(TABLE_SCHEMA, 20) TABLE_SCHEMA,
    left(TABLE_NAME, 45) TABLE_NAME,
    left(TABLE_TYPE, 10) TABLE_TYPE
FROM
    INFORMATION_SCHEMA.TABLES
WHERE
    TABLE_SCHEMA LIKE '%OWNER_PATTERN%'
    AND
    TABLE_NAME LIKE '%TABLE_PATTERN%'
go")



;; Find Column
(sql-set-product-feature
 'oracle
 :func-find-column "select
    table_name,
    column_name
    from
    all_tab_columns
    where
    column_name like '%PATTERN%'
	order by table_name, column_name;")

(sql-set-product-feature
 'ms
 :func-find-column "select top 100
		left(TABLE_SCHEMA, 10) TABLE_SCHEMA,
		left(TABLE_NAME, 30) TABLE_NAME,
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




;; --------------------------------------------------------------------------
;; Functions
;; --------------------------------------------------------------------------
(defun sql-eval ()
  "Evaluates SQL code."
  (interactive)
  ;; Pre Eval
  (when (eq (length (switch-frame-buffer-list '("\\*SQL.*") '("^ "))) 0)
    (call-interactively 'sql-connect)
    ;; Possible loop until process is found?
    )

  (unless sql-buffer
    (sql-set-sqli-buffer))

  (if (and transient-mark-mode mark-active)
      (sql-eval-region)
    (sql-eval-paragraph))
  (next-non-blank-line))

(defun sql-eval-region ()
  "Evaluates SQL region and returns back to current frame."
  (interactive)
  (save-frame-excursion
   (call-interactively 'sql-send-region))
  (deactivate-mark))

(defun sql-eval-paragraph ()
  "Evaluates SQL region and returns back to current frame."
  (interactive)
  (save-frame-excursion
   (call-interactively 'sql-send-paragraph))
  (forward-paragraph))

(defun switch-frame-next-sql ()
  "thisandthat."
  (interactive)
  (switch-frame-next-buffer '("\\*SQL") '("^ ")))

(defun switch-frame-previous-sql ()
  "Switch to previous SQL buffer."
  (interactive)
  (switch-frame-previous-buffer '("\\*SQL") '("^ ")))

(defun switch-frame-current-sql ()
  "thisandthat."
  (interactive)
  (display-buffer sql-buffer))

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
     
(advice-add 'sql-product-interactive :after #'(lambda (&rest args) (sql-eval-init)))

(defun sql-fix-indent ()
  "Fixes indents for a whole paragraph. Pretty much all one should need."
  (interactive)
  (save-excursion
    (progn
      (mark-paragraph)
      (call-interactively 'indent-region))))


;; -----------------------------------------------------------------------------
;; Help Functions
;;	- Vary by product / in progress
;; -----------------------------------------------------------------------------
(defun sql-describe ()
  "Describe the current table"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-desc)))
    (if func
	   (save-frame-excursion 
	    (save-excursion
		 (setq loc (+ (search-backward-regexp "[\\( \t\n\r]") 1)))
	    (save-excursion
		 (setq objname
			  (buffer-substring-no-properties
			   loc 
			   (- (search-forward-regexp "[\\) ,;\t\n\r]") 1))))
	    (sql-send-string (replace-regexp-in-string
					  "PATTERN"
					  (upcase objname)
					  func)))
	 (message "no :func-desc defined for product %s" sql-product))))

(defun sql-show-table ()
  "Lists the top elements of a table"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-show-table)))
    (if func
	   (save-frame-excursion 
	    (save-excursion
		 (setq loc (+ (search-backward-regexp "[\\( \t\n\r]") 1)))
	    (save-excursion
		 (setq objname
			  (buffer-substring-no-properties
			   loc 
			   (- (search-forward-regexp "[\\) ,;\t\n\r]") 1))))
	    (sql-send-string (replace-regexp-in-string
					  "TABLE"
					  (upcase objname)
					  func)))
	 (message "no :func-show-table defined for product %s" sql-product))))

(defun sql-inspect-table ()
  "Lists some table info"
  (interactive)
  (let ((func (sql-get-product-feature sql-product :func-inspect-table)))
    (if func
	   (save-frame-excursion 
	    (save-excursion
		 (setq loc (+ (search-backward-regexp "[\\( \t\n\r]") 1)))
	    (save-excursion
		 (setq objname
			  (buffer-substring-no-properties
			   loc 
			   (- (search-forward-regexp "[\\) ,;\t\n\r]") 1))))
	    (sql-send-string (replace-regexp-in-string
					  "TABLE_PATTERN"
					  (upcase objname)
					  func)))
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
  (if (not (save-excursion (forward-line 0) (eq (char-after) ?-)))
	 ;; Performs normal expansion.
	 (funcall expand)
    ;; We're inside a comment: use the text-mode abbrevs.
    (let ((local-abbrev-table text-mode-abbrev-table))
	 (funcall expand))))


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
      "prediction" "prediction_cost" "prediction_details"
      "prediction_probability" "prediction_set" "presentnnv" "presentv"
      "previous" "ref" "remainder" "stats_binomial_test" "stats_crosstab"
      "stats_f_test" "stats_ks_test" "stats_mode" "stats_mw_test"
      "stats_one_way_anova" "stats_t_test_indep" "stats_t_test_indepu"
      "stats_t_test_one" "stats_t_test_paired" "stats_wsr_test" "substr2"
      "substr4" "substrb" "substrc" "timestamp_to_scn" "to_binary_double"
      "to_binary_float" "value" "xmlparse" "xmlpi" "xmlquery" "xmlroot"
      "xmlserialize" "xmltable"))
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
  "lower" "map" "match" "map" "member" "merge" "method" "min" "minute"
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
  "year" "zone")
		)
  )

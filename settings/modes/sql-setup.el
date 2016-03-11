;; ----------------------------------------------------------------------------
;; SQL MODE
;; ----------------------------------------------------------------------------
(provide 'sql-setup)
(require 'sql)


;; ORACLE Path
(defvar SQL-ORACLE-Init-Path
"~/Public_Files/Hilbert/Common/SQL/sql defaults.sql"
"Path to Oracle Init File")

;; Servers
(require 'sql-servers "~/Documents/Keys/sql-servers.el")


(eval-after-load "sql" '(load-library "sql-indent")) 
(add-to-list 'ac-modes 'sql-mode)

(setq sql-ms-program		"sqlcmd"
      sql-oracle-program		"sqlplus"
	 sql-oracle-scan-on		nil
	 sql-send-terminator	nil		; since I don't put GO after
								; (CAUSE ISSUES IN SQLPLUS if non-nil)
	 sql-ms-options		'("-w" "80")
	 )

(sql-set-product-feature
 'postgres
 :func-desc "\\d+")

(sql-set-product-feature
 'postgres
 :func-show-table '("select * from" "limit 5"))

(sql-set-product-feature
 'oracle
 :func-desc "describe")



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
  
  (sql-set-product 'postgres)

  ;; (setq comment-start "/*") **/
  ;; (setq comment-end "*\/") **/
  )

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(defun my-sql-interactive-mode-hook ()
  (text-scale-set -1.1)
  
  (add-to-list 'ac-sources 'ac-source-sql)
  (auto-complete-mode t)
  ;; (setq ac-ignore-case nil)

  ;; (setq-default truncate-lines t)
  )

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

(defun sql-process-new ()
  "Creates a new SQL-ORACLE process."
  (interactive)
  (save-frame-excursion
   (sql-oracle)
   (sql-rename-buffer)
   (setq sql-buffer (buffer-name))		; Not sure why we need to set the sqli
								; buffer to it's self here
   ))

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
   (get-string-from-file f))
  )

(defun sql-eval-init ()
  "Evaluates SQL Init File."
  (interactive)
  (sql-eval-file SQL-ORACLE-Init-Path))

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
  (save-frame-excursion 
   (save-excursion
     (setq loc (+ (search-backward-regexp "[\\( \t\n\r]") 1)))
   (save-excursion
     (setq objname
	   (buffer-substring-no-properties
	    loc 
	    (- (search-forward-regexp "[\\) ,;\t\n\r]") 1))))
   (sql-send-string (concat (sql-get-product-feature sql-product :func-desc)
					   " "
					   objname ";"))))

(defun sql-show-table ()
  "Lists the top elements of a table"
  (interactive)
  (save-frame-excursion 
   (save-excursion
     (setq loc (+ (search-backward-regexp "[\\( \t\n\r]") 1)))
   (save-excursion
     (setq objname
	   (buffer-substring-no-properties
	    loc 
	    (- (search-forward-regexp "[\\) ,;\t\n\r]") 1))))
   (sql-send-string (concat
				 (car(sql-get-product-feature sql-product
										:func-show-table))
				 " "
				 objname
				 " "
				 (cadr(sql-get-product-feature sql-product
										 :func-show-table))
				 ";"))))

(defun sql-tables (name-pattern owner-pattern)
  (interactive "sName: \nsOwner: ")
  (sql-eval-init)
  (save-frame-excursion
   (sql-send-string
    (concat
     "SELECT * FROM
	(SELECT 'V' as type, owner, view_name as name
	FROM all_views
	UNION
	SELECT 'T' as type, owner, table_name as name
	FROM all_tables
        ORDER BY owner, name)
	WHERE owner LIKE '%" (upcase owner-pattern)"%'
        AND name LIKE '%" (upcase name-pattern) "%';"))
   (display-buffer sql-buffer)))

(defun sql-find-column (pattern)
  (interactive "sPattern: ")
  (sql-eval-init)
  (save-frame-excursion 
   (sql-send-string
    (concat
     "select
    table_name,
    column_name
    from
    all_tab_columns
    where
    column_name like '%" (upcase pattern) "%';"))))

(defun sql-user-tables (pattern)
  (interactive "sPattern: ")
  (sql-eval-init)
  (save-frame-excursion 
   (sql-send-string (concat 
    "SELECT
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
	table_name like '%" (upcase pattern) "%'
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
	max_bytes > 0;"))))

(defun sql-user-functions ()
  (interactive)
  (save-frame-excursion 
   (sql-send-string
    "SELECT
	object_name,
	object_type,
	status
     FROM
	ALL_OBJECTS
     WHERE
	OBJECT_TYPE
	IN ('FUNCTION','PROCEDURE') and
	owner = user;")))

(defun sql-last-error ()
  (interactive)
  (save-frame-excursion 
   (sql-send-region
 "set underline off;
select *
from SYS.USER_ERRORS
WHERE rownum = 1
ORDER BY rownum DESC;
set underline on;")))

(defun sql-explain ()
  "Explain plan of code"
  (interactive)
  (save-frame-excursion 
   ;; (sql-send-string "SET LINESIZE 60;\n")
   (sql-send-string "explain plan for (")
   (call-interactively 'sql-send-region)
   (sql-send-string ");")
   (sql-send-string "select plan_table_output
from table(dbms_xplan.display('plan_table',null,'basic +cost'))
union all
select plan_table_output
from table(dbms_xplan.display('plan_table',null,'basic +bytes +rows'))
union all
select plan_table_output from
table(dbms_xplan.display('plan_table',null,'typical -cost -bytes -rows -partition -parallel +PREDICATE +note'));")
   ;; (sql-send-string "SET LINESIZE 3000;\n")
   ))


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


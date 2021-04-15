;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
				 '(("when" "        when\n        ${1:CONDITION}\n        then ${2:TRUE_VALUE}\n	$0" "when" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/when.yasnippet" nil nil)
				   ("to_date" "to_date('$1', '${2:MM/DD/YYYY}')$0" "to_date" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/to_date.yasnippet" nil nil)
				   ("sqluldr" "-- Output --\n    host sqluldr ${1:TABLE} \"&this_path/../data/$1.csv\"" "sqluldr" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/sqluldr.yasnippet" nil nil)
				   ("rownum" "rownum <= ${1:5}$0" "rownum" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/rownum" nil nil)
				   ("over" "over (partition by $1 order by $2) $0" "over" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/over.yasnippet" nil nil)
				   ("over" "OVER	(\n	${1:PARTITION BY $2}${3: ORDER BY $4}) $0\n" "over" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/over" nil nil)
				   ("header" "/*-----------------------------------------------------------------------------\n- `(upcase (file-name-nondirectory (buffer-file-name)))`\n-------------------------------------------------------------------------------\n	${1:Simple description}\n\n	AUTHOR:		`user-full-name`\n	CREATED:		`(format-time-string \"%Y-%m-%d\")`\n	MODIFIED:		`(format-time-string \"%Y-%m-%d\")`\n	\n-------------------------------------------------------------------------------\n-- SUMMARY \n-------------------------------------------------------------------------------\n	$3\n\n-------------------------------------------------------------------------------\n-- REVISIONS\n-------------------------------------------------------------------------------\n	1.0	- creation\n				 -------------------------------------------------------------------------------\n-- DEPENDENCIES\n-------------------------------------------------------------------------------\n	- ${4:None}\n \n-------------------------------------------------------------------------------\n-- TO DO\n-------------------------------------------------------------------------------\n	- ${5:None}\n \n-------------------------------------------------------------------------------\n-- EXAMPLES\n-------------------------------------------------------------------------------\n	- None\n\n-----------------------------------------------------------------------------*/\n    \n$0\n" "header" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/header.yasnippet" nil nil)
				   ("date" "DATE '${1:YYYY}-${2:MM}-${3:DD}'$0" "date" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/date" nil nil)
				   ("CASE" "    (CASE\n        WHEN\n        ${1:CONDITION}\n        THEN ${2:TRUE_VALUE}\n        ELSE ${3:ELSE_VALUE}\n        END) $0\n" "CASE" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/sql-mode/case.yasnippet" nil nil)))


;;; Do not edit! File generated at Wed Apr 14 16:24:50 2021

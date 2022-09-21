explain plan for (REGION);
	select plan_table_output
	from table(dbms_xplan.display('plan_table',null,'basic +cost'))
	union all
	select plan_table_output
	from table(dbms_xplan.display('plan_table',null,'basic +bytes +rows'))
	union all
	select plan_table_output from
	table(dbms_xplan.display('plan_table',null,'typical -cost -bytes -rows
-partition -parallel +PREDICATE +note'));
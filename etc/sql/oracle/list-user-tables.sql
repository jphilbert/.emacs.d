SELECT
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
	max_bytes > 0;
SELECT
  ROUND(SUM(bytes)/(1024*1024),1) AS size_mb,
  SUM(NUM_ROWS) AS n_rows,
  MAX(created) AS created,
  CAST(COMMENTS AS VARCHAR2(20)) AS comments
FROM
  user_extents
  JOIN
  all_tables ON
    segment_name = TABLE_NAME
  JOIN
  all_objects ON
    TABLE_NAME = object_name
  LEFT JOIN
  user_tab_comments USING(TABLE_NAME)
WHERE
  segment_type = 'TABLE'
	AND
  TABLE_NAME = '${table}'
GROUP BY
  TABLE_NAME,
  COMMENTS
ORDER BY
  TABLE_NAME

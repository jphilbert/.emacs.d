SELECT
  t AS TYPE,
  SUBSTR(owner || '.' || NAME, 1, 70) AS NAME
FROM (
  SELECT
    'VW' AS T, owner, view_name AS NAME
  FROM
    all_views
  UNION
  SELECT
    'TBL' AS T, owner, TABLE_NAME AS NAME
  FROM
    all_tables)
WHERE
  owner LIKE '${schema}'
    AND
  NAME LIKE '${table}'  
ORDER BY
  2, 1

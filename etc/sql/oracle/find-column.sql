SELECT
  SUBSTR(owner || '.' || TABLE_NAME, 1, 48) AS TABLE_NAME,
  SUBSTR(COLUMN_NAME, 1, 28)
FROM
  all_tab_columns
WHERE
  COLUMN_NAME LIKE '${column}'
    AND
  TABLE_NAME LIKE '${table}'
ORDER BY
  owner, TABLE_NAME, COLUMN_NAME

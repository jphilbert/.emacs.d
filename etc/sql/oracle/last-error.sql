SET underline OFF;
SELECT
  *
FROM
  SYS.USER_ERRORS
WHERE
  rownum = 1
ORDER BY
  rownum DESC;
SET underline ON;

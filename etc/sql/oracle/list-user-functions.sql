SELECT
	object_name,
	object_type,
	status
     FROM
	ALL_OBJECTS
     WHERE
	OBJECT_TYPE
	IN ('FUNCTION','PROCEDURE') and
	owner = user;
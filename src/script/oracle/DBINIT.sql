-- USER SQL
CREATE USER EADM IDENTIFIED BY '123';

-- ADD ROLES
GRANT CONNECT TO EADM;
GRANT CONSOLE_DEVELOPER TO EADM;
GRANT DWROLE TO EADM;
GRANT GRAPH_DEVELOPER TO EADM;
GRANT OML_DEVELOPER TO EADM;
GRANT RESOURCE TO EADM;
ALTER USER EADM DEFAULT ROLE CONSOLE_DEVELOPER,DWROLE,GRAPH_DEVELOPER,OML_DEVELOPER;

-- REST ENABLE
BEGIN
    ORDS_ADMIN.ENABLE_SCHEMA(
        p_enabled => TRUE,
        p_schema => 'EADM',
        p_url_mapping_type => 'BASE_PATH',
        p_url_mapping_pattern => 'eadm',
        p_auto_rest_auth=> TRUE
    );
    -- ENABLE DATA SHARING
    C##ADP$SERVICE.DBMS_SHARE.ENABLE_SCHEMA(
            SCHEMA_NAME => 'EADM',
            ENABLED => TRUE
    );
    commit;
END;
/

-- ENABLE GRAPH
ALTER USER EADM GRANT CONNECT THROUGH GRAPH$PROXY_USER;

-- ENABLE OML
ALTER USER EADM GRANT CONNECT THROUGH OML$PROXY;

-- QUOTA
ALTER USER EADM QUOTA UNLIMITED ON DATA;





{src/adm2/globals.i}

&GLOBAL-DEFINE loginname admin
&GLOBAL-DEFINE password  

DEFINE INPUT  PARAMETER pcMode                      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pdCurrentUserObj            AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentUserLogin          AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentUserName           AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentUserEmail          AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pdCurrentOrganisationObj    AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentOrganisationCode   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentOrganisationName   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentOrganisationShort  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER ptCurrentProcessDate        AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER pdCurrentLanguageObj        AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentLanguageName       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER pcCurrentLoginValues        AS CHARACTER  NO-UNDO.


DEFINE VARIABLE cFailedReason AS CHARACTER  NO-UNDO.

RUN checkUser IN gshSecurityManager (INPUT "{&loginname}",
                                     INPUT IF "{&password}" <> "":U THEN ENCODE("{&password}") ELSE "":U,
                                     INPUT 0,
                                     INPUT 0,
                                     OUTPUT pdCurrentUserObj,
                                     OUTPUT pcCurrentUserName,
                                     OUTPUT pcCurrentUserEmail,
                                     OUTPUT pcCurrentOrganisationCode,
                                     OUTPUT pcCurrentOrganisationName,
                                     OUTPUT pcCurrentOrganisationShort,
                                     OUTPUT pcCurrentLanguageName,
                                     OUTPUT cFailedReason).

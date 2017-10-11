&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Check Version Notes Wizard" Procedure _INLINE
/* Actions: af/cod/aftemwizcw.w ? ? ? ? */
/* MIP Update Version Notes Wizard
Check object version notes.
af/cod/aftemwizpw.w
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Update-Object-Version" Procedure _INLINE
/* Actions: ? ? ? ? af/sup/afverxftrp.p */
/* This has to go above the definitions sections, as that is what it modifies.
   If its not, then the definitions section will have been saved before the
   XFTR code kicks in and changes it */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Definition Comments Wizard" Procedure _INLINE
/* Actions: ? af/cod/aftemwizcw.w ? ? ? */
/* Program Definition Comment Block Wizard
Welcome to the Program Definition Comment Block Wizard. Press Next to proceed.
af/cod/aftemwizpw.w
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*---------------------------------------------------------------------------------
  File: importprof.p

  Description:  import profile data into temp-tables

  Purpose:

  Parameters:   <none>

  History:
  --------
  (v:010000)    Task:           0   UserRef:    
                Date:   03/14/2002  Author:     mattB

  Update Notes: Created from Template rytemprocp.p

---------------------------------------------------------------------------------*/
/*                   This .W file was created with the Progress UIB.             */
/*-------------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* MIP-GET-OBJECT-VERSION pre-processors
   The following pre-processors are maintained automatically when the object is
   saved. They pull the object and version from Roundtable if possible so that it
   can be displayed in the about window of the container */

&scop object-name       importprof.p
DEFINE VARIABLE lv_this_object_name AS CHARACTER INITIAL "{&object-name}":U NO-UNDO.
&scop object-version    000000


/* object identifying preprocessor */
&glob   AstraProcedure    yes

{src/adm2/globals.i}

  {PROFILER/profile.i}

  DEFINE INPUT  PARAMETER iSessionId   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER inpfile      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cDescription AS CHARACTER  NO-UNDO.

  DEFINE OUTPUT PARAMETER TABLE FOR ttSOURCE.
  DEFINE OUTPUT PARAMETER TABLE FOR calltree.
  DEFINE OUTPUT PARAMETER TABLE FOR sumstmt.
  DEFINE OUTPUT PARAMETER TABLE FOR profile-session.
  DEFINE OUTPUT PARAMETER TABLE FOR calltreedata.
  DEFINE OUTPUT PARAMETER TABLE FOR listing-lines.
  DEFINE OUTPUT PARAMETER TABLE FOR executable-line.
  DEFINE OUTPUT PARAMETER TABLE FOR traceinfo.
  DEFINE OUTPUT PARAMETER TABLE FOR USER-DATA.

  DEFINE VARIABLE cNumericFormat AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 13.38
         WIDTH              = 56.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define buffer sessionsource for ttsource.
define buffer parentsource  for ttsource.
define buffer lsource       for ttsource.

define var parentname     as char no-undo.
define var sname          as char no-undo.
define var sid            as int no-undo.
define var versno         as int no-undo.
DEFINE var savedateformat as char no-undo.


IF SEARCH(inpfile) = ? THEN
  RETURN ERROR "Could not find profiler data file.".

cNumericFormat = SESSION:NUMERIC-FORMAT.
SESSION:NUMERIC-FORMAT = "AMERICAN".

IMPORTBLOCK:
DO ON ERROR UNDO, LEAVE IMPORTBLOCK
   ON QUIT UNDO, LEAVE IMPORTBLOCK
   ON STOP UNDO, LEAVE IMPORTBLOCK:
  /* this is here to trap errors so we don't hose the numeric format */
  RUN ImportData NO-ERROR.

END.



/* we're done */
PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Done.").

SESSION:NUMERIC-FORMAT = cNumericFormat.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-importcalltreedata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importcalltreedata Procedure 
PROCEDURE importcalltreedata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE caller       AS INTEGER    NO-UNDO.
DEFINE VARIABLE callerlineno AS INTEGER    NO-UNDO.
DEFINE VARIABLE callee       AS INTEGER    NO-UNDO.
DEFINE VARIABLE callcnt      AS INTEGER    NO-UNDO.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Call Tree Data").


/* now read in all the calltree information */
REPEAT ON ENDKEY UNDO, LEAVE:
  IMPORT caller callerlineno callee callcnt.

  create calltreedata.
  ASSIGN 
    calltreedata.session-id   = profile-session.session-id
    calltreedata.caller       = caller
    calltreedata.callerlineno = callerlineno
    calltreedata.callee       = callee
    calltreedata.callcnt      = callcnt.

end.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Resolving Call Tree Data").

for each calltreedata of profile-session:
   find first calltree of calltreedata no-error.
   if not available(calltree) then
      buffer-copy calltreedata to calltree.
   else
      calltree.callcnt = calltree.callcnt + calltreedata.callcnt.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ImportData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportData Procedure 
PROCEDURE ImportData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
input from value(inpfile).

RUN importprofilesession NO-ERROR.


FIND FIRST profile-session NO-ERROR.

IF NOT AVAILABLE profile-session THEN DO:
  MESSAGE 'no valid sessions'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.
  
RUN importsource NO-ERROR.

RUN importcalltreedata NO-ERROR.


RUN importsumstmt NO-ERROR.

if versno GE 1 then do:
  /* for version 0, we didn't have this information */


  RUN importtraceinfo.

  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Coverage Information").
  /* read coverage analysis information */
  coverblk:
  repeat:
    import sid sname ^.
    if sname NE "" then do:


        /* we've got a procedure, function, or trigger -- find it's parent */
        find parentsource where
                parentsource.session-id = profile-session.session-id
            AND parentsource.srcid = sid NO-ERROR.
        if not available(parentsource) then do:
           /*run skipcoverage. */
           undo coverblk, next coverblk.
        end.

        sname = parentsource.srcname + " " + sname.
        find ttsource where ttsource.session-id = profile-session.session-id
                AND ttsource.srcname EQ sname NO-ERROR.
        if not available(ttsource) then do:
           /* this source was not executed during this profiling session */
           find last lsource where
                     lsource.session-id = profile-session.session-id.
           create ttsource.
           assign ttsource.srcid = lsource.srcid + 1
                  ttsource.session-id = lsource.session-id
                  ttsource.srcname = sname.
        end.
        sid = ttsource.srcid.
    end.
    RUN importexeclines.

  end.

  RUN importuserdata.
end. /* end if versno GE 1 */

input close.


PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Calculating Statistics").
/* now massage the data */
for each ttSOURCE OF profile-session:
  
  /* establish how many times this source was called */
  for each calltree where calltree.session-id = profile-session.session-id
                          AND calltree.callee = ttsource.srcid:
    ttsource.callcnt = ttsource.callcnt + calltree.callcnt.
  end.
  
  /* establish what the "parent" procedure is, if there is one */
  parentname = entry(2, ttsource.srcname, " ") no-error.
  if NOT error-status:error then
  do:
    find first parentsource
         where parentsource.session-id = profile-session.session-id
               AND parentsource.srcname = parentname no-error.
    if available parentsource then
      assign ttsource.parent = parentsource.srcid
             ttsource.listname = parentsource.listname.
  end.
  
  /* compute the total actual time and average actual time for the procedure */
  for each sumstmt of ttsource:
    assign
      ttsource.tot_acttime = ttsource.tot_acttime + sumstmt.tot_acttime
      sumstmt.parent = if ttsource.parent EQ 0 then ttsource.srcid else ttsource.parent.
  end.
  if ttsource.callcnt GT 0 then /* may be zero if only have coverage analysis
                               * info on the source, but no execution during
                               * this profiling session */
    ttsource.avg_acttime = ttsource.tot_acttime / ttsource.callcnt.
end.

  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Calculating Runtime Percentages").

/* figure out the total session time so we can compute percentages */
  find sumstmt where sumstmt.session-id = profile-session.session-id
                  AND sumstmt.srcid = 0
                  AND sumstmt.lineno = 0 no-error.
  if not available(sumstmt) then
    find sumstmt where sumstmt.session-id = profile-session.session-id
                  AND sumstmt.srcid = 1
                  AND sumstmt.lineno = 0 no-error.
  if not available(sumstmt) then
  do:
    message "Can't summarize data. Cannot locate summary statement for the Session."
            view-as alert-box.
    return.
  end.
  
  profile-session.tot_acttime = sumstmt.cumtime.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Calculating Source Percentages").
/* calculate runtime percentages */
FOR EACH ttsource of profile-session:
   ASSIGN
      ttsource.session-percent = 
         ttsource.tot_acttime * 100.0 / profile-session.tot_acttime
      ttsource.percall-percent = 
         ttsource.avg_acttime * 100.0 / profile-session.tot_acttime.

   FOR EACH sumstmt OF ttsource:
     
     ASSIGN 
       sumstmt.session-percent = sumstmt.acttime * 100.0 / profile-session.tot_acttime.

     IF ttsource.avg_acttime GT 0 THEN
        sumstmt.perprocedure-percent =
           sumstmt.tot_acttime * 100.0 / ttsource.callcnt / ttsource.avg_acttime.
   END.
   FIND FIRST sumstmt where sumstmt.session-id = ttsource.session-id
                        AND sumstmt.srcid = ttsource.srcid
                        AND sumstmt.lineno GE 0 no-error.
   IF avail(sumstmt) and sumstmt.lineno EQ 0 THEN DO:
     ASSIGN 
      ttsource.overhead_time  = sumstmt.acttime
      ttsource.tot_cumtime    = sumstmt.tot_cumtime.
      FIND NEXT sumstmt where sumstmt.session-id = ttsource.session-id
                          AND sumstmt.srcid = ttsource.srcid
                          AND sumstmt.lineno GE 0 NO-ERROR.
   END.
   if available(sumstmt) then
      ttsource.first-line = sumstmt.lineno.
   else ttsource.first-line = 1.
  ttSource.Total-Time = ttsource.avg_acttime * ttsource.callcnt.
END.

/* calculate procedure exec times */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importexeclines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importexeclines Procedure 
PROCEDURE importexeclines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE line-no AS INTEGER    NO-UNDO.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Executable Lines").

    /* now read in what executable lines there are for this source */
    repeat:
      IMPORT line-no.

      create executable-line.
      assign executable-line.session-id = profile-session.session-id
             executable-line.srcid = sid
            executable-line.line-no = line-no.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importprofilesession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importprofilesession Procedure 
PROCEDURE importprofilesession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE sesdate AS DATE       NO-UNDO.
  DEFINE VARIABLE sesdesc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE sestime AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE sesuser  AS CHARACTER  NO-UNDO.
    
PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Session Profile").
sessionblock:
REPEAT ON ENDKEY UNDO, LEAVE sessionblock:
  
  ASSIGN 
    savedateformat               = SESSION:DATE-FORMAT.

  import versno sesdate sesdesc sestime sesuser.

  if versno NE 0 AND versno NE 1 then do:
     message "Cannot load data. Don't know how to load data for version" versno
           view-as alert-box.
     undo, return.
  end.

  create profile-session.
  ASSIGN 
    iSessionId                   = iSessionId
    profile-session.session-id   = iSessionId
    session-notes                = cDescription
    SESSION:DATE-FORMAT          = "mdy"
    profile-session.session-date = sesdate
    profile-Session.Session-Desc = sesdesc
    Profile-Session.Session-Time = sestime
    Profile-Session.Session-User = sesuser
    SESSION:DATE-FORMAT          = savedateformat.

  create sessionsource. /* create source id 0 -- the session "source" */
  assign sessionsource.session-id = profile-session.session-id
         sessionsource.srcid      = 0
         sessionsource.srcname    = "Session"
         sessionsource.callcnt    = 1
         sessionsource.listname   = "" NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
    DELETE sessionsource.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importSource) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importSource Procedure 
PROCEDURE importSource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* read in all the source information */
DEFINE VARIABLE srcid    AS INTEGER    NO-UNDO.
DEFINE VARIABLE srcname  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE listname AS CHARACTER  NO-UNDO.
DEFINE VARIABLE crcval   AS INTEGER    NO-UNDO.

DEFINE VARIABLE cextension AS CHARACTER  NO-UNDO.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Source").

REPEAT ON ENDKEY UNDO, LEAVE:
  IMPORT srcid srcname listname crcval.

  IF NOT CAN-FIND(FIRST ttsource WHERE 
                  ttSOURCE.session-id = profile-session.session-id AND
                  ttSOURCE.srcid = srcid) THEN DO:
    create ttsource.
    ASSIGN
      ttsource.session-id = profile-session.session-id
      ttsource.srcid      = srcid
      ttsource.srcname    = srcname
      ttsource.listname   = listname
      ttsource.CRC-VAL    = crcval.
  END.

  IF LENGTH(ttsource.srcname) > 2 THEN 
    cExtension = SUBSTRING(ttsource.srcname, LENGTH(ttsource.srcname) - 1) NO-ERROR.

  IF cExtension > "" AND INDEX(".w,.p", cExtension) > 0 THEN DO:
    IF R-INDEX(ttsource.srcname, " ") > 0  THEN
      ttsource.srcfile = SUBSTRING(ttsource.srcname, R-INDEX(ttsource.srcname, " ") + 1).
    ELSE
      ttsource.srcfile = ttsource.srcname.
  END.


end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importsumstmt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importsumstmt Procedure 
PROCEDURE importsumstmt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE srcid       AS INTEGER    NO-UNDO.
DEFINE VARIABLE lineno      AS INTEGER    NO-UNDO.
DEFINE VARIABLE istmtcnt    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cumtime     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE stmtcnt     AS INTEGER    NO-UNDO.
DEFINE VARIABLE tot_acttime AS DECIMAL    NO-UNDO.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Summary Statements").

/* now read in all the summary statements */
sumstmtblock:
REPEAT:
  IMPORT srcid lineno stmtcnt tot_acttime cumtime.

  create sumstmt.
  ASSIGN 
    sumstmt.session-id   = profile-session.session-id
    sumstmt.srcid        = srcid
    sumstmt.lineno       = lineno
    sumstmt.stmtcnt      = stmtcnt
    sumstmt.tot_acttime  = tot_acttime
    sumstmt.cumtime      = cumtime
    sumstmt.tot_cumtime  = cumtime.     

  assign sumstmt.acttime = sumstmt.tot_acttime / sumstmt.stmtcnt
         sumstmt.cumtime = sumstmt.cumtime / sumstmt.stmtcnt.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importtraceinfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importtraceinfo Procedure 
PROCEDURE importtraceinfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE srcid AS INTEGER    NO-UNDO.
DEFINE VARIABLE line-no AS INTEGER    NO-UNDO.
DEFINE VARIABLE acttime AS DECIMAL    NO-UNDO.
DEFINE VARIABLE starttime AS DECIMAL    NO-UNDO.


PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing Trace Information").
  /* read tracing information */
  repeat:
    IMPORT srcid line-no acttime starttime.

    create traceinfo.
    ASSIGN 
      traceinfo.session-id   = profile-session.session-id
      traceinfo.srcid        = srcid
      traceinfo.line-no      = line-no
      traceinfo.acttime      = acttime
      traceinfo.starttime    = starttime.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-importuserdata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importuserdata Procedure 
PROCEDURE importuserdata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE data       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE event-time AS DECIMAL    NO-UNDO.

PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Importing User Data").

  /* read any user data */
  repeat:
    IMPORT event-time data.

    create user-data.
    ASSIGN 
      user-data.session-id = profile-session.session-id
      user-data.event-time  = event-time
      user-data.data        = data.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


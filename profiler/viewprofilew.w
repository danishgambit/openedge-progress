&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wiWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Update-Object-Version" wiWin _INLINE
/* Actions: ? ? ? ? af/sup/afverxftrp.p */
/* This has to go above the definitions sections, as that is what it modifies.
   If its not, then the definitions section will have been saved before the
   XFTR code kicks in and changes it */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Definition Comments Wizard" wiWin _INLINE
/* Actions: ? af/cod/aftemwizcw.w ? ? ? */
/* Program Definition Comment Block Wizard
Welcome to the Program Definition Comment Block Wizard. Press Next to proceed.
af/cod/aftemwizpw.w
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wiWin 
/*---------------------------------------------------------------------------------
  File: smartprofilew.w

  Description:  

  Purpose:

  Parameters:   <none>

  History:
  --------
  (v:010000)    Task:           0   UserRef:    
                Date:   03/19/2002  Author:     mattB

  Update Notes: Created from Template rysttbconw.w
                Created from Template smartprofilew.w

---------------------------------------------------------------------------------*/
/*                   This .W file was created with the Progress UIB.             */
/*-------------------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* MIP-GET-OBJECT-VERSION pre-processors
   The following pre-processors are maintained automatically when the object is
   saved. They pull the object and version from Roundtable if possible so that it
   can be displayed in the about window of the container */

&scop object-name       viewprofilew.w
DEFINE VARIABLE lv_this_object_name AS CHARACTER INITIAL "{&object-name}":U NO-UNDO.
&scop object-version    000000

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* object identifying preprocessor */
&glob   astra2-staticSmartWindow yes
&SCOPED-DEFINE WholeIndexBackColor  12
/*  {src/adm2/globals.i} */

/* DEFINE STREAM stXRef. */
DEFINE STREAM stFile.


    {PROFILER/profile.i SHARED }
    {PROFILER/idxchk.i}

    DEFINE VARIABLE iSessionId AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSrcId     AS INTEGER    NO-UNDO.


    DEFINE BUFFER ttbsource   FOR ttsource.
    DEFINE BUFFER ttbcalltree FOR calltree.
    DEFINE BUFFER ttbcallee   FOR ttsource.
    DEFINE BUFFER ttbcaller   FOR ttsource.

    DEFINE VARIABLE cIndexNameShown  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFileNameShown   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iLineNumberShown AS INTEGER    NO-UNDO.
    DEFINE VARIABLE deTotalTime      AS DECIMAL    NO-UNDO DECIMALS 6.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME frMain
&Scoped-define BROWSE-NAME brCallee

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttbcalltree ttbcallee calltree ttbcaller ~
sumstmt ttSearch ttsource

/* Definitions for BROWSE brCallee                                      */
&Scoped-define FIELDS-IN-QUERY-brCallee ttbcallee.srcname ttbcallee.callcnt ttbcallee.session-percent   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCallee   
&Scoped-define SELF-NAME brCallee
&Scoped-define QUERY-STRING-brCallee FOR EACH ttbcalltree WHERE   ttbcalltree.session-id = iSessionId and   ttbcalltree.caller = iSrcId, ~
         EACH ttbcallee WHERE     ttbcallee.session-id = ttbcalltree.session-id AND     ttbcallee.srcid = ttbcalltree.callee   BY ttbcalltree.callcnt DESCENDING
&Scoped-define OPEN-QUERY-brCallee OPEN QUERY {&SELF-NAME} FOR EACH ttbcalltree WHERE   ttbcalltree.session-id = iSessionId and   ttbcalltree.caller = iSrcId, ~
         EACH ttbcallee WHERE     ttbcallee.session-id = ttbcalltree.session-id AND     ttbcallee.srcid = ttbcalltree.callee   BY ttbcalltree.callcnt DESCENDING.
&Scoped-define TABLES-IN-QUERY-brCallee ttbcalltree ttbcallee
&Scoped-define FIRST-TABLE-IN-QUERY-brCallee ttbcalltree
&Scoped-define SECOND-TABLE-IN-QUERY-brCallee ttbcallee


/* Definitions for BROWSE brCaller                                      */
&Scoped-define FIELDS-IN-QUERY-brCaller ttbcaller.srcname ttbcaller.callcnt ttbcaller.session-percent   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCaller   
&Scoped-define SELF-NAME brCaller
&Scoped-define QUERY-STRING-brCaller FOR EACH calltree WHERE   calltree.session-id = iSessionId and   calltree.callee = iSrcId, ~
         EACH ttbcaller WHERE     ttbcaller.session-id = calltree.session-id AND     ttbcaller.srcid = calltree.caller   BY calltree.callcnt DESCENDING
&Scoped-define OPEN-QUERY-brCaller OPEN QUERY {&SELF-NAME} FOR EACH calltree WHERE   calltree.session-id = iSessionId and   calltree.callee = iSrcId, ~
         EACH ttbcaller WHERE     ttbcaller.session-id = calltree.session-id AND     ttbcaller.srcid = calltree.caller   BY calltree.callcnt DESCENDING.
&Scoped-define TABLES-IN-QUERY-brCaller calltree ttbcaller
&Scoped-define FIRST-TABLE-IN-QUERY-brCaller calltree
&Scoped-define SECOND-TABLE-IN-QUERY-brCaller ttbcaller


/* Definitions for BROWSE brLine                                        */
&Scoped-define FIELDS-IN-QUERY-brLine sumstmt.lineno sumstmt.stmtcnt sumstmt.acttime sumstmt.tot_acttime sumstmt.tot_cumtime ttSearch.IndexScore1 ttSearch.IndexScore2 ttSearch.DatabaseName ttSearch.TableName cFileNameShown iLineNumberShown   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brLine   
&Scoped-define SELF-NAME brLine
&Scoped-define QUERY-STRING-brLine FOR EACH sumstmt WHERE   sumstmt.session-id = iSessionId and   sumstmt.srcid = iSrcId, ~
         EACH ttSearch OUTER-JOIN WHERE     ttsearch.srcid = sumstmt.srcid AND     ttsearch.LineId = sumstmt.lineno   BY sumstmt.tot_acttime DESCENDING
&Scoped-define OPEN-QUERY-brLine OPEN QUERY {&SELF-NAME} FOR EACH sumstmt WHERE   sumstmt.session-id = iSessionId and   sumstmt.srcid = iSrcId, ~
         EACH ttSearch OUTER-JOIN WHERE     ttsearch.srcid = sumstmt.srcid AND     ttsearch.LineId = sumstmt.lineno   BY sumstmt.tot_acttime DESCENDING.
&Scoped-define TABLES-IN-QUERY-brLine sumstmt ttSearch
&Scoped-define FIRST-TABLE-IN-QUERY-brLine sumstmt
&Scoped-define SECOND-TABLE-IN-QUERY-brLine ttSearch


/* Definitions for BROWSE brSource                                      */
&Scoped-define FIELDS-IN-QUERY-brSource ttsource.srcname ttsource.callcnt ttsource.avg_acttime deTotalTime ttsource.session-percent ttsource.tot_cumtime   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSource   
&Scoped-define SELF-NAME brSource
&Scoped-define QUERY-STRING-brSource FOR EACH ttsource WHERE   ttsource.session-id = isessionid   BY ttsource.session-id BY ttsource.tot_acttime DESCENDING BY ttsource.srcname
&Scoped-define OPEN-QUERY-brSource OPEN QUERY {&SELF-NAME} FOR EACH ttsource WHERE   ttsource.session-id = isessionid   BY ttsource.session-id BY ttsource.tot_acttime DESCENDING BY ttsource.srcname.
&Scoped-define TABLES-IN-QUERY-brSource ttsource
&Scoped-define FIRST-TABLE-IN-QUERY-brSource ttsource


/* Definitions for FRAME frMain                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS buCompare coSessions brSource brCaller ~
brCallee brLine RECT-4 rectpercent 
&Scoped-Define DISPLAYED-OBJECTS coSessions fidate fitime filength 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fct_RemoveString wiWin 
FUNCTION fct_RemoveString RETURNS CHARACTER
  ( piLine AS CHAR, piString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalleeQuery wiWin 
FUNCTION getCalleeQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCallerQuery wiWin 
FUNCTION getCallerQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPercentQuery wiWin 
FUNCTION getPercentQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSourceQuery wiWin 
FUNCTION getSourceQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTimeQuery wiWin 
FUNCTION getTimeQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wiWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_profilecodev AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON buCompare 
     LABEL "&Compare" 
     CONTEXT-HELP-ID 0
     SIZE 15 BY 1.14.

DEFINE VARIABLE coSessions AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE fidate AS DATE FORMAT "99/99/9999":U 
     LABEL "Date" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE filength AS DECIMAL FORMAT ">>>>,>>9.9999":U INITIAL 0 
     LABEL "Total Run Time" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fitime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 163 BY 1.43.

DEFINE RECTANGLE rectpercent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 7.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCallee FOR 
      ttbcalltree, 
      ttbcallee SCROLLING.

DEFINE QUERY brCaller FOR 
      calltree, 
      ttbcaller SCROLLING.

DEFINE QUERY brLine FOR 
      sumstmt, 
      ttSearch SCROLLING.

DEFINE QUERY brSource FOR 
      ttsource SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCallee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCallee wiWin _FREEFORM
  QUERY brCallee DISPLAY
      ttbcallee.srcname         FORMAT "X(150)"      COLUMN-LABEL  "Called Code Block"
ttbcallee.callcnt         FORMAT ">>,>>9"     COLUMN-LABEL  "Calls To"
ttbcallee.session-percent FORMAT ">>9.999999" COLUMN-LABEL  "% Session"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 64 BY 3.33 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.

DEFINE BROWSE brCaller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCaller wiWin _FREEFORM
  QUERY brCaller DISPLAY
      ttbcaller.srcname         FORMAT "X(150)"  COLUMN-LABEL  "Calling Code Block"
  ttbcaller.callcnt         FORMAT ">>,>>9" COLUMN-LABEL "Calls From"
  ttbcaller.session-percent FORMAT ">>9.999999" COLUMN-LABEL "% Session"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 64 BY 3.81 FIT-LAST-COLUMN.

DEFINE BROWSE brLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brLine wiWin _FREEFORM
  QUERY brLine DISPLAY
      sumstmt.lineno       COLUMN-LABEL "Line"       FORMAT ">>>,>>9"
  sumstmt.stmtcnt      COLUMN-LABEL "Exec Count" FORMAT ">>>,>>9"
  sumstmt.acttime      COLUMN-LABEL "Avg Exec"   FORMAT ">,>>9.999999"
  sumstmt.tot_acttime  COLUMN-LABEL "Tot Time"   FORMAT  ">,>>9.999999"
  sumstmt.tot_cumtime  COLUMN-LABEL "Cum Time"   FORMAT ">,>>>9.999999"
  ttSearch.IndexScore1  FORMAT "ZZ9.9%"  COLUMN-LABEL "Score#1"
  ttSearch.IndexScore2  FORMAT "ZZ9.9%"  COLUMN-LABEL "Score#2"
  ttSearch.DatabaseName FORMAT "X(12)"   COLUMN-LABEL "Database"
  ttSearch.TableName    FORMAT "X(25)"   COLUMN-LABEL "Table"
  cFileNameShown        FORMAT "X(37)"   COLUMN-LABEL "FileName"
  iLineNumberShown      FORMAT ">>>>>9"  COLUMN-LABEL "Line"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 163 BY 5.71 FIT-LAST-COLUMN.

DEFINE BROWSE brSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSource wiWin _FREEFORM
  QUERY brSource DISPLAY
      ttsource.srcname         FORMAT "X(150)"      COLUMN-LABEL "Code Block"
 ttsource.callcnt         FORMAT ">>,>>9"     COLUMN-LABEL "Calls To"
 ttsource.avg_acttime     FORMAT ">>9.999999" COLUMN-LABEL "Avg Time"
 deTotalTime              FORMAT ">,>>9.999999"     COLUMN-LABEL "Tot Time"
 ttsource.session-percent FORMAT ">>9.999999" COLUMN-LABEL "% Session"
 ttsource.tot_cumtime     FORMAT ">>>>,>>>9.999999" COLUMN-LABEL "Cum Time"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 7.38 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     buCompare AT ROW 1.14 COL 148
     coSessions AT ROW 1.24 COL 2 NO-LABEL
     fidate AT ROW 1.24 COL 43 COLON-ALIGNED
     fitime AT ROW 1.24 COL 68 COLON-ALIGNED
     filength AT ROW 1.24 COL 100 COLON-ALIGNED
     brSource AT ROW 2.67 COL 1
     brCaller AT ROW 2.67 COL 100
     brCallee AT ROW 6.71 COL 100
     brLine AT ROW 10.29 COL 1
     RECT-4 AT ROW 1 COL 1
     rectpercent AT ROW 2.67 COL 97
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 163.8 BY 25.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wiWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Profile Viewer"
         HEIGHT             = 25.19
         WIDTH              = 163.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 163.8
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 163.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wiWin:LOAD-ICON("adeicon/icfrt.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/icfrt.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wiWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wiWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frMain
                                                                        */
/* BROWSE-TAB brSource filength frMain */
/* BROWSE-TAB brCaller brSource frMain */
/* BROWSE-TAB brCallee brCaller frMain */
/* BROWSE-TAB brLine brCallee frMain */
ASSIGN 
       brCallee:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

ASSIGN 
       brCaller:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

ASSIGN 
       brLine:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

ASSIGN 
       brSource:ALLOW-COLUMN-SEARCHING IN FRAME frMain = TRUE
       brSource:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

/* SETTINGS FOR COMBO-BOX coSessions IN FRAME frMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fidate IN FRAME frMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filength IN FRAME frMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fitime IN FRAME frMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wiWin)
THEN wiWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCallee
/* Query rebuild information for BROWSE brCallee
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttbcalltree WHERE
  ttbcalltree.session-id = iSessionId and
  ttbcalltree.caller = iSrcId,
  EACH ttbcallee WHERE
    ttbcallee.session-id = ttbcalltree.session-id AND
    ttbcallee.srcid = ttbcalltree.callee
  BY ttbcalltree.callcnt DESCENDING.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brCallee */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCaller
/* Query rebuild information for BROWSE brCaller
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH calltree WHERE
  calltree.session-id = iSessionId and
  calltree.callee = iSrcId,
  EACH ttbcaller WHERE
    ttbcaller.session-id = calltree.session-id AND
    ttbcaller.srcid = calltree.caller
  BY calltree.callcnt DESCENDING.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brCaller */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brLine
/* Query rebuild information for BROWSE brLine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH sumstmt WHERE
  sumstmt.session-id = iSessionId and
  sumstmt.srcid = iSrcId,
  EACH ttSearch OUTER-JOIN WHERE
    ttsearch.srcid = sumstmt.srcid AND
    ttsearch.LineId = sumstmt.lineno
  BY sumstmt.tot_acttime DESCENDING.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brLine */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSource
/* Query rebuild information for BROWSE brSource
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttsource WHERE
  ttsource.session-id = isessionid
  BY ttsource.session-id BY ttsource.tot_acttime DESCENDING BY ttsource.srcname.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brSource */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frMain
/* Query rebuild information for FRAME frMain
     _Query            is NOT OPENED
*/  /* FRAME frMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wiWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiWin wiWin
ON END-ERROR OF wiWin /* Profile Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiWin wiWin
ON WINDOW-CLOSE OF wiWin /* Profile Viewer */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCallee
&Scoped-define SELF-NAME brCallee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCallee wiWin
ON MOUSE-SELECT-DBLCLICK OF brCallee IN FRAME frMain
DO:
    RUN changeSource (INPUT ttbcallee.srcid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCaller
&Scoped-define SELF-NAME brCaller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCaller wiWin
ON MOUSE-SELECT-DBLCLICK OF brCaller IN FRAME frMain
DO:
  RUN changeSource (INPUT ttbcaller.srcid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brLine
&Scoped-define SELF-NAME brLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brLine wiWin
ON ROW-DISPLAY OF brLine IN FRAME frMain
DO:

  FOR FIRST ttXRef FIELDS (srcid xFileName xLineNumber xMisc2) WHERE 
    ttxref.srcid = sumstmt.srcid and
    ttXRef.LineId = ttSearch.XRef-LineId NO-LOCK:

  ASSIGN cFileNameShown   = ""
         iLineNumberShown = 0.
  ASSIGN cFileNameShown   = (IF INDEX(ttXRef.xFileName,"\":u) > 0
                                THEN SUBSTRING(ttXRef.xFileName,R-INDEX(ttXRef.xFileName,"\":u) + 1)
                                ELSE ttXRef.xFileName)
            iLineNumberShown = ttXRef.xLineNumber.
     IF ttXRef.xMisc2 = "WHOLE-INDEX":u AND NUM-ENTRIES(ttSearch.AccessedFields) > 0
     THEN ASSIGN ttSearch.IndexScore1:BGCOLOR  IN BROWSE brLine = {&WholeIndexBackColor}
                 ttSearch.IndexScore2:BGCOLOR  = {&WholeIndexBackColor}
                 ttSearch.DatabaseName:BGCOLOR = {&WholeIndexBackColor}
                 ttSearch.TableName:BGCOLOR    = {&WholeIndexBackColor}    
                 cFileNameShown:BGCOLOR        = {&WholeIndexBackColor}             
                 iLineNumberShown:BGCOLOR      = {&WholeIndexBackColor}.  
  END. /* First ttXRef */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brLine wiWin
ON VALUE-CHANGED OF brLine IN FRAME frMain
DO:
   RUN changeLine.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSource
&Scoped-define SELF-NAME brSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSource wiWin
ON ROW-DISPLAY OF brSource IN FRAME frMain
DO:
  deTotalTime = ttsource.avg_acttime * ttsource.callcnt.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSource wiWin
ON VALUE-CHANGED OF brSource IN FRAME frMain
DO:
    RUN changeSource (INPUT ttSource.srcId).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buCompare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buCompare wiWin
ON CHOOSE OF buCompare IN FRAME frMain /* Compare */
DO:
  RUN CompareProfileData.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME coSessions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL coSessions wiWin
ON VALUE-CHANGED OF coSessions IN FRAME frMain
DO:
  DEFINE VARIABLE iSessionId AS INTEGER    NO-UNDO.
  ASSIGN
    coSessions.
  iSessionId = INTEGER(coSessions).
  
  RUN changeSession (iSessionId) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Session does not exist"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCallee
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wiWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wiWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'profiler/profilecodev.w':U ,
             INPUT  FRAME frMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_profilecodev ).
       RUN repositionObject IN h_profilecodev ( 16.24 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 9.52 , 162.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_profilecodev ,
             brLine:HANDLE IN FRAME frMain , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeLine wiWin 
PROCEDURE changeLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iLineNo      AS INTEGER    NO-UNDO.

  
  IF AVAILABLE sumstmt THEN
    iLineNo = sumstmt.lineno.
  ELSE
    iLineNo = 0.

  {&open-query-brIndex}

  RUN changeLine IN h_profilecodev (iLineNo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeSession wiWin 
PROCEDURE changeSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER iNewSessionId AS INTEGER    NO-UNDO.

  FIND FIRST profile-session WHERE
    profile-session.session-id = iNewSessionId
    NO-ERROR.
  
  IF NOT AVAILABLE profile-session THEN DO:
    MESSAGE 'could not find session' inewsessionid
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR.
  END.

  ASSIGN
    iSessionId = profile-session.session-id.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      fidate:SCREEN-VALUE    = STRING(profile-session.session-date)
      fitime:SCREEN-VALUE    = profile-session.session-time
      filength:SCREEN-VALUE  = STRING(profile-session.tot_acttime)
      coSessions             = STRING(iSessionId)
      .
    DISPLAY coSessions.
  END.

  RUN openQueries.

  APPLY "value-changed" TO brSource.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeSource wiWin 
PROCEDURE changeSource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER piSrcId AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cFile   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hqsource AS HANDLE     NO-UNDO.

  DEFINE BUFFER ttbrsource FOR ttsource.

  SESSION:SET-WAIT-STATE("general":u).
    
  iSrcId = piSrcId.

  DO WITH FRAME {&FRAME-NAME}:
    IF piSrcId <> ttsource.srcid THEN DO:
      FIND FIRST ttbrsource WHERE
        ttbrsource.srcid = pisrcid
        NO-ERROR.    
      IF AVAILABLE (ttbrsource) THEN DO:
        hqsource = brsource:QUERY.
        hqsource:REPOSITION-TO-ROWID (ROWID(ttbrsource)).
      END.

    END.
  
    {&OPEN-QUERY-brLine}
  
    {&OPEN-QUERY-brCaller}
  
    {&OPEN-QUERY-brCallee}

    RUN changeSource  IN h_profilecodev (ttSource.listname). 
  
    APPLY "value-changed" TO brLine.
  END.

  SESSION:SET-WAIT-STATE("":u).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompareProfileData wiWin 
PROCEDURE CompareProfileData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE hCompare AS HANDLE     NO-UNDO.

  RUN profiler/compprof.w PERSISTENT SET hCompare.
  IF valid-handle(hCompare) THEN DO:
    RUN initializeObject IN hCompare.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wiWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wiWin)
  THEN DELETE WIDGET wiWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wiWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY coSessions fidate fitime filength 
      WITH FRAME frMain IN WINDOW wiWin.
  ENABLE buCompare coSessions brSource brCaller brCallee brLine RECT-4 
         rectpercent 
      WITH FRAME frMain IN WINDOW wiWin.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW wiWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wiWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wiWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hColumn  AS HANDLE     NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  RUN selectPage ( INPUT 1 ).

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      coSessions:LIST-ITEM-PAIRS = ",".
    coSessions:DELETE(1).    
  END.
  
  FIND FIRST profile-session NO-LOCK NO-ERROR.

  IF NOT AVAILABLE profile-session THEN DO:
      MESSAGE "No Sessions are available to view"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
  coSessions = STRING(profile-session.session-id).


  DO WITH FRAME {&FRAME-NAME}:

    FOR EACH profile-session:
      ASSIGN
        iSessionId = profile-session.session-id.
      coSessions:ADD-LAST(STRING(profile-session.session-id) + " " + profile-session.Session-Desc, 
                          STRING(profile-session.session-id)).  
    END.
    DISPLAY coSessions.
  END.


  DO WITH FRAME {&FRAME-NAME}:
    hColumn = brsource:GET-BROWSE-COLUMN(1).
    hColumn:WIDTH = 36.
    hcolumn = brCallee:GET-BROWSE-COLUMN (1).
    hColumn:WIDTH = 39.
    hcolumn = brCaller:GET-BROWSE-COLUMN (1).
    hColumn:WIDTH = 39.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries wiWin 
PROCEDURE openQueries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:  
    {&open-query-brsource}
    APPLY "value-changed" TO brsource.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startSearch wiWin 
PROCEDURE startSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phBrowse AS HANDLE  NO-UNDO.

  DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
  
  DEFINE VARIABLE hQuery AS HANDLE     NO-UNDO.


  hColumn = phBrowse:CURRENT-COLUMN.

  hQuery = phBrowse:QUERY.

  APPLY 'END-SEARCH':U TO phBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fct_RemoveString wiWin 
FUNCTION fct_RemoveString RETURNS CHARACTER
  ( piLine AS CHAR, piString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WHILE INDEX(piLine,piString) > 0:
     piLine = TRIM(REPLACE(piLine,piString," ":u)).
  END.
  RETURN piLine.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalleeQuery wiWin 
FUNCTION getCalleeQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUBSTITUTE("for each ttbcalltree WHERE ttbcalltree.session-id = &1" +
      " and ttbcalltree.caller = &2," +
      " EACH ttbcallee WHERE ttbcallee.session-id = ttbcalltree.session-id " +
      " AND ttbcallee.srcid = ttbcalltree.callee " +
      " BY ttbcalltree.callcnt DESCENDING", iSessionId, iSrcId).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCallerQuery wiWin 
FUNCTION getCallerQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUBSTITUTE ("for each calltree WHERE calltree.session-id = &1" + 
      " and calltree.callee = &2," +
      " EACH ttbcaller WHERE ttbcaller.session-id = calltree.session-id" +
      " AND ttbcaller.srcid = calltree.caller " +
      " BY calltree.callcnt DESCENDING", iSessionId, iSrcId).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPercentQuery wiWin 
FUNCTION getPercentQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUBSTITUTE("for each ttbsource where ttbsource.session-id = &1" + 
    " and (( &2 GT 0 and ttbsource.parent = &2) or ttbsource.srcid = &2)" +
    " by ttbsource.session-percent descending", iSessionId, iSrcId).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSourceQuery wiWin 
FUNCTION getSourceQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUBSTITUTE("for each ttsource where ttsource.session-id = &1" +
    " by ttsource.session-id by tot_acttime descending by srcname", iSessionId).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTimeQuery wiWin 
FUNCTION getTimeQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUBSTITUTE("FOR EACH sumstmt WHERE sumstmt.session-id = &1" + 
    " and sumstmt.srcid = &2 BY sumstmt.tot_acttime DESCENDING",
    iSessionId, iSrcId).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


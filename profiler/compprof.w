&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: compprof.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{profiler/profile.i SHARED}

DEFINE VARIABLE httResult AS HANDLE     NO-UNDO.
DEFINE VARIABLE hbrResult AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQResult  AS HANDLE     NO-UNDO.

DEFINE BUFFER b1ttSource FOR ttSource.
DEFINE BUFFER b2ttSource FOR ttSource.

DEFINE STREAM sFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS coSessions1 coSessions2 coSort 
&Scoped-Define DISPLAYED-OBJECTS coSessions1 coSessions2 coSort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Export       LABEL "&Export"       
              DISABLED
       MENU-ITEM m_Exit         LABEL "E&xit"         .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_File         LABEL "&File"         .


/* Definitions of the field level widgets                               */
DEFINE VARIABLE coSessions1 AS CHARACTER FORMAT "X(90)":U 
     CONTEXT-HELP-ID 0
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE coSessions2 AS CHARACTER FORMAT "X(90)":U 
     CONTEXT-HELP-ID 0
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE coSort AS CHARACTER FORMAT "X(90)":U 
     LABEL "Sort" 
     CONTEXT-HELP-ID 0
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1,1" 
     DROP-DOWN-LIST
     SIZE 39 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     coSessions1 AT ROW 1 COL 1 NO-LABEL
     coSessions2 AT ROW 1 COL 60 COLON-ALIGNED NO-LABEL
     coSort AT ROW 1 COL 112 COLON-ALIGNED
     "<-- Compare -->" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 168.6 BY 22.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Compare Profiles"
         HEIGHT             = 22.05
         WIDTH              = 168.6
         MAX-HEIGHT         = 22.05
         MAX-WIDTH          = 168.6
         VIRTUAL-HEIGHT     = 22.05
         VIRTUAL-WIDTH      = 168.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/icfrt.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/icfrt.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR COMBO-BOX coSessions1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Compare Profiles */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Compare Profiles */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Compare Profiles */
DO:
  RUN resizeWindow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME coSessions1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL coSessions1 C-Win
ON VALUE-CHANGED OF coSessions1 IN FRAME DEFAULT-FRAME
DO:
  RUN CompareResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME coSessions2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL coSessions2 C-Win
ON VALUE-CHANGED OF coSessions2 IN FRAME DEFAULT-FRAME
DO:
  RUN CompareResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME coSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL coSort C-Win
ON VALUE-CHANGED OF coSort IN FRAME DEFAULT-FRAME /* Sort */
DO:

  ASSIGN
    cosort.

  RUN SortBrowse (INPUT cosort).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Export C-Win
ON CHOOSE OF MENU-ITEM m_Export /* Export */
DO:
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO INIT "c:\temp\session_export.csv".

  RUN profiler/getname.p (INPUT-OUTPUT cFileName).


  IF cFileName <> ? AND cFileName <> "" THEN
    RUN exportResults (INPUT cFileName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildResultBrowse C-Win 
PROCEDURE BuildResultBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE hColumn AS HANDLE     NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  
CREATE BROWSE hbrResult
  ASSIGN 
   COL                    = 1
   ROW                    = coSessions1:HEIGHT + 1
   WIDTH                  = {&WINDOW-NAME}:WIDTH
   HEIGHT                 = {&WINDOW-NAME}:HEIGHT - coSessions1:HEIGHT - .5
   ALLOW-COLUMN-SEARCHING = TRUE
   COLUMN-MOVABLE         = FALSE
   COLUMN-RESIZABLE       = TRUE
   COLUMN-SCROLLING       = TRUE
   FIT-LAST-COLUMN        = TRUE
   FRAME                  = FRAME {&FRAME-NAME}:HANDLE
   READ-ONLY              = TRUE
   ROW-MARKERS            = FALSE
   SEPARATORS             = TRUE
   QUERY                  = hQResult
  .
END.

DO iCount = 1 TO httResult:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
  hbrResult:ADD-LIKE-COLUMN(httResult:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iCount)).
END.

hColumn = hbrResult:GET-BROWSE-COLUMN(1).
hColumn:WIDTH = 36.
hColumn = hbrResult:GET-BROWSE-COLUMN(3).
hcolumn:COLUMN-FGCOLOR = 1.
hColumn = hbrResult:GET-BROWSE-COLUMN(5).
hcolumn:COLUMN-FGCOLOR = 1.
hColumn = hbrResult:GET-BROWSE-COLUMN(7).
hcolumn:COLUMN-FGCOLOR = 1.
hColumn = hbrResult:GET-BROWSE-COLUMN(9).
hcolumn:COLUMN-FGCOLOR = 1.
hColumn = hbrResult:GET-BROWSE-COLUMN(11).
hcolumn:COLUMN-FGCOLOR = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildResultQuery C-Win 
PROCEDURE BuildResultQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE hResultBuf AS HANDLE     NO-UNDO.

IF NOT VALID-HANDLE(hQResult) THEN DO:
  CREATE QUERY hQResult.
END.

hQResult:QUERY-CLOSE().

hResultBuf = httResult:DEFAULT-BUFFER-HANDLE.

hQResult:SET-BUFFERS(hResultBuf).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildResultTable C-Win 
PROCEDURE BuildResultTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE TEMP-TABLE httResult.

httResult:ADD-NEW-FIELD("source_name", "character", 1, "X(150)", "", "Source Name").
httResult:ADD-NEW-FIELD("callsto1", "integer", 1, ">,>>9.99", 0, "Calls To 1").
httResult:ADD-NEW-FIELD("callsto2", "integer", 1, ">,>>9.99", 0, "Calls To 2").
httResult:ADD-NEW-FIELD("avg1", "decimal", 1, ">>9.999999", 0, "Avg Time 1").
httResult:ADD-NEW-FIELD("avg2", "decimal", 1, ">>9.999999", 0, "Avg Time 2").
httResult:ADD-NEW-FIELD("Tot1", "decimal", 1, ">,>>9.999999", 0, "Tot Time 1").
httResult:ADD-NEW-FIELD("Tot2", "decimal", 1, ">,>>9.999999", 0, "Tot Time 2").
httResult:ADD-NEW-FIELD("percent1", "decimal", 1, ">>9.999999", 0, "Sess % 1").
httResult:ADD-NEW-FIELD("percent2", "decimal", 1, ">>9.999999", 0, "Sess % 2").
httResult:ADD-NEW-FIELD("cum1", "decimal", 1, ">>>>,>>>9.999999", 0, "Cum Time 1").
httResult:ADD-NEW-FIELD("cum2", "decimal", 1, ">>>>,>>>9.999999", 0, "Cum Time 2").
httResult:ADD-NEW-INDEX("iname").
httResult:ADD-INDEX-FIELD("iname", "source_name").

httResult:TEMP-TABLE-PREPARE("ttResult").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildSortList C-Win 
PROCEDURE BuildSortList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE hResultBuf AS HANDLE     NO-UNDO.

DEFINE VARIABLE cLabel AS CHARACTER  NO-UNDO.

hResultBuf = httResult:DEFAULT-BUFFER-HANDLE.
DO WITH FRAME {&FRAME-NAME}:
  coSort:LIST-ITEM-PAIRS = "1,1".
  coSort:DELETE(1).
  
END.


DO iCount = 1 TO hResultBuf:NUM-FIELDS:

  DO WITH FRAME {&FRAME-NAME}:
    coSort:ADD-LAST(hResultBuf:BUFFER-FIELD(iCount):LABEL, 
                    hresultbuf:BUFFER-FIELD(iCount):NAME).
  END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompareResults C-Win 
PROCEDURE CompareResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE hResultBuf AS HANDLE     NO-UNDO.
DEFINE VARIABLE lOk        AS LOGICAL    NO-UNDO.

DEFINE VARIABLE cQueryString AS CHARACTER  NO-UNDO 
  INIT "for each ttResult by source_name":U.

hQResult:QUERY-CLOSE().

hResultBuf = httResult:DEFAULT-BUFFER-HANDLE.

hResultBuf:EMPTY-TEMP-TABLE().

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    coSessions1 coSessions2.
END.

FIND FIRST b1ttSource WHERE b1ttSource.session-id = INTEGER(coSessions1).
FIND FIRST b2ttSource WHERE b2ttSource.session-id = INTEGER(coSessions2).

IF NOT AVAILABLE b1ttSource OR NOT AVAILABLE b2ttSource THEN RETURN.

SESSION:SET-WAIT-STATE("GENERAL":U).

FOR EACH b1ttSource WHERE b1ttSource.session-id = INTEGER(coSessions1):
  lOk = hResultBuf:FIND-FIRST("where source_name = ~"" + b1ttSource.srcname + "~"") NO-ERROR.
  IF NOT lOk THEN
    hResultBuf:BUFFER-CREATE().
  ASSIGN 
    hResultBuf:BUFFER-FIELD("source_name":U):BUFFER-VALUE  = b1ttSource.srcname
    hResultBuf:BUFFER-FIELD("callsto1":U):BUFFER-VALUE     = b1ttSource.callcnt
    hResultBuf:BUFFER-FIELD("avg1":U):BUFFER-VALUE         = b1ttSource.avg_acttime
    hResultBuf:BUFFER-FIELD("tot1":U):BUFFER-VALUE         = b1ttSource.total-time
    hResultBuf:BUFFER-FIELD("percent1":U):BUFFER-VALUE     = b1ttSource.session-percent
    hResultBuf:BUFFER-FIELD("cum1":U):BUFFER-VALUE         = b1ttSource.tot_cumtime.
  
END.


FOR EACH b2ttSource WHERE b2ttSource.session-id = INTEGER(coSessions2):
  lOk = hResultBuf:FIND-FIRST("where source_name = ~"" + b2ttSource.srcname + "~"") NO-ERROR.
  IF NOT lOk THEN
    hResultBuf:BUFFER-CREATE().
  ASSIGN 
    hResultBuf:BUFFER-FIELD("source_name":U):BUFFER-VALUE  = b2ttSource.srcname
    hResultBuf:BUFFER-FIELD("callsto2":U):BUFFER-VALUE     = b2ttSource.callcnt
    hResultBuf:BUFFER-FIELD("avg2":U):BUFFER-VALUE         = b2ttSource.avg_acttime
    hResultBuf:BUFFER-FIELD("tot2":U):BUFFER-VALUE         = b2ttSource.total-time
    hResultBuf:BUFFER-FIELD("percent2":U):BUFFER-VALUE     = b2ttSource.session-percent
    hResultBuf:BUFFER-FIELD("cum2":U):BUFFER-VALUE         = b2ttSource.tot_cumtime.
  
END.

hQResult:QUERY-PREPARE(cQueryString).
hQResult:QUERY-OPEN().

ASSIGN
  hbrResult:SENSITIVE = TRUE
  hbrResult:VISIBLE   = TRUE.     

RUN buildSortList.

MENU-ITEM m_export:SENSITIVE  IN MENU MENU-BAR-C-Win  = TRUE.


SESSION:SET-WAIT-STATE("":U).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY coSessions1 coSessions2 coSort 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE coSessions1 coSessions2 coSort 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportResults C-Win 
PROCEDURE exportResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcExportFile AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hResultBuf AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField     AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.


  hResultBuf = httResult:DEFAULT-BUFFER-HANDLE.
  
  CREATE QUERY hQuery.
  
  hQuery:SET-BUFFERS(hResultBuf).
  hQuery:QUERY-PREPARE("for each " + hResultBuf:NAME).
  
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  
  OUTPUT STREAM sFile TO VALUE(pcExportFile).
  
    /* export column headers */
    DO iCount = 1 TO hResultBuf:NUM-FIELDS:
      hfield = hresultbuf:BUFFER-FIELD(icount).
      PUT STREAM sFile UNFORMATTED hField:NAME ",".
    END.
  
    PUT STREAM sFile UNFORMATTED SKIP.
  
  DO WHILE NOT hQuery:QUERY-OFF-END:
  
    DO iCount = 1 TO hResultBuf:NUM-FIELDS:
      hfield = hresultbuf:BUFFER-FIELD(icount).
      PUT STREAM sFile UNFORMATTED hField:BUFFER-VALUE ",".
    END.
  
    PUT STREAM sFile UNFORMATTED SKIP.
  
    hQuery:GET-NEXT().
  END.
  
  OUTPUT STREAM sFile CLOSE.
  
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject C-Win 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iSessionID AS INTEGER    NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      coSessions1:LIST-ITEM-PAIRS = ","
      coSessions2:LIST-ITEM-PAIRS = ",".
    coSessions1:DELETE(1).
    coSessions2:DELETE(1).
  END.

  ASSIGN 
    {&WINDOW-NAME}:MIN-HEIGHT = 5
    {&WINDOW-NAME}:MIN-WIDTH  = 100
    {&WINDOW-NAME}:MAX-HEIGHT = SESSION:HEIGHT
    {&WINDOW-NAME}:MAX-WIDTH = SESSION:WIDTH.

  
  FIND FIRST profile-session NO-LOCK NO-ERROR.

  IF NOT AVAILABLE profile-session THEN DO:
      MESSAGE "No Sessions are available to view"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN 
    coSessions1 = STRING(profile-session.session-id).
    coSessions2 = STRING(profile-session.session-id).


  DO WITH FRAME {&FRAME-NAME}:

    FOR EACH profile-session:
      ASSIGN
        iSessionId = profile-session.session-id.
      coSessions1:ADD-LAST(STRING(profile-session.session-id) + " " + profile-session.Session-Desc, 
                          STRING(profile-session.session-id)).  
    END.
    coSessions2:LIST-ITEM-PAIRS = coSessions1:LIST-ITEM-PAIRS.
    DISPLAY coSessions1.
    DISPLAY coSessions2.
  END.

  RUN BuildResultTable.
  RUN BuildResultQuery.
  RUN BuildResultBrowse.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow C-Win 
PROCEDURE resizeWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hFrame AS HANDLE     NO-UNDO.
                      
hFrame = FRAME {&FRAME-NAME}:HANDLE.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    hFrame:HEIGHT = {&WINDOW-NAME}:HEIGHT
    hFrame:WIDTH  = {&WINDOW-NAME}:WIDTH NO-ERROR.
  ASSIGN
    hbrResult:HEIGHT = hFrame:HEIGHT - coSessions1:HEIGHT - .5
    hbrResult:WIDTH  = hFrame:WIDTH NO-ERROR.
  ASSIGN
    hFrame:HEIGHT = {&WINDOW-NAME}:HEIGHT
    hFrame:WIDTH  = {&WINDOW-NAME}:WIDTH
    NO-ERROR.
  
  ASSIGN
    hbrResult:HEIGHT = hFrame:HEIGHT - coSessions1:HEIGHT - .5
    hbrResult:WIDTH  = hFrame:WIDTH NO-ERROR.

END.

hFrame:VIRTUAL-HEIGHT = hFrame:HEIGHT.
hFrame:VIRTUAL-WIDTH  = hFrame:WIDTH.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortBrowse C-Win 
PROCEDURE SortBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER cColName AS CHARACTER    NO-UNDO.

DEFINE VARIABLE cQueryString AS CHARACTER  NO-UNDO 
  INIT "for each ttresult".

hQResult:QUERY-CLOSE().

IF cColName > "" THEN
  cQueryString = cQueryString + " by " + cColName.

IF cColName <> "source_name" THEN
  cqueryString = cQueryString + " desc".


hQResult:QUERY-PREPARE(cQueryString).

hQResult:QUERY-OPEN().




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: profile.p

  Description: PRO*Tool to control profile session

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

{PROFILER/profile.i NEW SHARED}

{src/adm2/globals.i}

DEFINE VARIABLE iSessionId     AS INTEGER    NO-UNDO.
DEFINE VARIABLE hRunContainer   AS HANDLE     NO-UNDO.
DEFINE VARIABLE hViewer         AS HANDLE     NO-UNDO.

DEFINE VARIABLE lServerDisabled AS LOGICAL    NO-UNDO.

DEFINE STREAM sFile.

DEFINE TEMP-TABLE ttRunObject NO-UNDO
  FIELD ObjectHandle AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS filesbtn filesbtn-2 raProfile raRunType ~
fiRunCode ckClearCache btnProfile btnStop listing-tgl coverage-tgl ~
Tracing-tgl ckClient profdesc outpfilename directory filter-fillin ckServer ~
profdesc-2 outpfilename-2 directory-2 filter-fillin-2 ckAutoWrite toClear ~
ToLoad btnWrite buview RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS raProfile raRunType fiRunCode ckClearCache ~
listing-tgl coverage-tgl Tracing-tgl ckClient profdesc outpfilename ~
directory filter-fillin ckServer profdesc-2 outpfilename-2 directory-2 ~
filter-fillin-2 ckAutoWrite toClear ToLoad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnProfile 
     LABEL "&Run" 
     CONTEXT-HELP-ID 0
     SIZE 14 BY 1.14 TOOLTIP "Program or object to run".

DEFINE BUTTON btnStop 
     LABEL "&Stop" 
     CONTEXT-HELP-ID 0
     SIZE 14 BY 1.19 TOOLTIP "Stop all running procedures".

DEFINE BUTTON btnWrite 
     LABEL "&Write Data" 
     CONTEXT-HELP-ID 0
     SIZE 13 BY 1.14 TOOLTIP "Export profile data to specified files".

DEFINE BUTTON buview 
     LABEL "&View" 
     SIZE 13 BY 1.14 TOOLTIP "View Profile Data"
     BGCOLOR 8 .

DEFINE BUTTON filesbtn  NO-FOCUS
     LABEL "..." 
     SIZE 3 BY .81.

DEFINE BUTTON filesbtn-2  NO-FOCUS
     LABEL "..." 
     SIZE 3 BY .81.

DEFINE VARIABLE directory AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter the name of the directory of profiler listing files." NO-UNDO.

DEFINE VARIABLE directory-2 AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter the name of the directory of profiler listing files." NO-UNDO.

DEFINE VARIABLE filter-fillin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter a comma separated list of filters for tracing." NO-UNDO.

DEFINE VARIABLE filter-fillin-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter a comma separated list of filters for tracing." NO-UNDO.

DEFINE VARIABLE fiRunCode AS CHARACTER FORMAT "X(50)":U 
     LABEL "Procedure" 
     CONTEXT-HELP-ID 0
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Procedure or Dynamics object to run" NO-UNDO.

DEFINE VARIABLE outpfilename AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 TOOLTIP "Enter the output file name for the Profiling data." NO-UNDO.

DEFINE VARIABLE outpfilename-2 AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 TOOLTIP "Enter the output file name for the Profiling data." NO-UNDO.

DEFINE VARIABLE profdesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter the description of the profiling session" NO-UNDO.

DEFINE VARIABLE profdesc-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 TOOLTIP "Enter the description of the profiling session" NO-UNDO.

DEFINE VARIABLE raProfile AS CHARACTER 
     CONTEXT-HELP-ID 0
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Run", "Run",
"Debug", "Debug",
"Profile", "Profile"
     SIZE 12 BY 2.14 TOOLTIP "Select Code Profiling or Code Debugging" NO-UNDO.

DEFINE VARIABLE raRunType AS CHARACTER 
     CONTEXT-HELP-ID 0
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "&Procedure", "&Procedure",
"&Container", "&Container"
     SIZE 32.6 BY .71 TOOLTIP "Choose Procedure or Container to run or launch" NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 6.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 3.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 6.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 8.33.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 8.33.

DEFINE VARIABLE ckAutoWrite AS LOGICAL INITIAL yes 
     LABEL "Write On Stop" 
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Automatically write Profile Data when stop button is pressed" NO-UNDO.

DEFINE VARIABLE ckClearCache AS LOGICAL INITIAL no 
     LABEL "Clear Client Cache" 
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 TOOLTIP "Clear Dynamics client-side object cache" NO-UNDO.

DEFINE VARIABLE ckClient AS LOGICAL INITIAL yes 
     LABEL "Client" 
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 TOOLTIP "Enable profiling of client-side execution" NO-UNDO.

DEFINE VARIABLE ckServer AS LOGICAL INITIAL yes 
     LABEL "Application Server" 
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 TOOLTIP "Enable profiling of server-side execution" NO-UNDO.

DEFINE VARIABLE coverage-tgl AS LOGICAL INITIAL no 
     LABEL "Coverage" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 TOOLTIP "Generate coverage analysis information." NO-UNDO.

DEFINE VARIABLE listing-tgl AS LOGICAL INITIAL no 
     LABEL "Listings" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 TOOLTIP "Generate listing files." NO-UNDO.

DEFINE VARIABLE toClear AS LOGICAL INITIAL no 
     LABEL "Clear Profiles" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Delete all cached profile data from memory" NO-UNDO.

DEFINE VARIABLE ToLoad AS LOGICAL INITIAL yes 
     LABEL "Re Load Profile" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 TOOLTIP "Empty profile data cache and reload from profiler files" NO-UNDO.

DEFINE VARIABLE Tracing-tgl AS LOGICAL INITIAL no 
     LABEL "Tracing" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 TOOLTIP "Generate procedure tracing information." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filesbtn AT ROW 12.05 COL 35.6
     filesbtn-2 AT ROW 12.05 COL 75.8
     raProfile AT ROW 1.71 COL 64.8 NO-LABEL
     raRunType AT ROW 1.67 COL 3 NO-LABEL
     fiRunCode AT ROW 2.62 COL 12 COLON-ALIGNED
     ckClearCache AT ROW 3.86 COL 14
     btnProfile AT ROW 2.67 COL 46
     btnStop AT ROW 4.1 COL 46
     listing-tgl AT ROW 4.33 COL 65
     coverage-tgl AT ROW 5.29 COL 65
     Tracing-tgl AT ROW 6.24 COL 65
     ckClient AT ROW 7.91 COL 3
     profdesc AT ROW 10.05 COL 3 NO-LABEL
     outpfilename AT ROW 11.95 COL 3 NO-LABEL
     directory AT ROW 13.86 COL 3 NO-LABEL
     filter-fillin AT ROW 15.76 COL 3 NO-LABEL
     ckServer AT ROW 7.91 COL 42
     profdesc-2 AT ROW 10.05 COL 43 NO-LABEL
     outpfilename-2 AT ROW 11.95 COL 43 NO-LABEL
     directory-2 AT ROW 13.86 COL 43 NO-LABEL
     filter-fillin-2 AT ROW 15.76 COL 43 NO-LABEL
     ckAutoWrite AT ROW 17.91 COL 43
     toClear AT ROW 18.86 COL 43
     ToLoad AT ROW 19.81 COL 43
     btnWrite AT ROW 17.91 COL 65
     buview AT ROW 19.33 COL 65
     RECT-2 AT ROW 1.24 COL 63
     RECT-3 AT ROW 17.43 COL 41
     RECT-4 AT ROW 1.29 COL 2
     RECT-5 AT ROW 8.86 COL 2
     RECT-6 AT ROW 8.86 COL 41
     "Client Profile Description" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 9.33 COL 3
     "Client Output File" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 3
     "Client Listing Files Directory" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 13.14 COL 3
     "Client Filter for Tracing" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 15.05 COL 3
     "Run Code" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1 COL 3.8
     "Server Profile Description" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 9.33 COL 43
     "Server Output File" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 43
     "Server Listing Files Directory" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 13.14 COL 43
     "Server Filter for Tracing" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 15.05 COL 43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Profiler Control"
         HEIGHT             = 20.48
         WIDTH              = 80.6
         MAX-HEIGHT         = 32.19
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 32.19
         VIRTUAL-WIDTH      = 204.8
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

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
   Size-to-Fit Custom                                                   */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN directory IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN directory-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN filter-fillin IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN filter-fillin-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN outpfilename IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN outpfilename-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN profdesc IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN profdesc-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Profiler Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */

  RUN ProfilerOff NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
    RETURN NO-APPLY.

  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Profiler Control */
DO:

  RUN ProfilerOff NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
    RETURN NO-APPLY.

      
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProfile C-Win
ON CHOOSE OF btnProfile IN FRAME DEFAULT-FRAME /* Run */
DO:
  DEBUGGER:CLEAR().

  RUN assignFieldValues.

  RUN CheckInput NO-ERROR.

  EXECUTE-BLOCK:   
  DO ON ERROR  UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK  
     ON ENDKEY UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK
     ON STOP   UNDO EXECUTE-BLOCK, LEAVE EXECUTE-BLOCK
     ON QUIT                     , LEAVE EXECUTE-BLOCK:

    IF raProfile = "Debug" THEN
      RUN DebugCode NO-ERROR.
    ELSE
      RUN ProfileCode NO-ERROR.

  END.

  IF RETURN-VALUE > "" THEN
    MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStop C-Win
ON CHOOSE OF btnStop IN FRAME DEFAULT-FRAME /* Stop */
DO:

  RUN StopContainer NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWrite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWrite C-Win
ON CHOOSE OF btnWrite IN FRAME DEFAULT-FRAME /* Write Data */
DO:
  RUN writeProfileData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buview C-Win
ON CHOOSE OF buview IN FRAME DEFAULT-FRAME /* View */
DO:

  RUN ViewProfileData.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ckClient
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ckClient C-Win
ON VALUE-CHANGED OF ckClient IN FRAME DEFAULT-FRAME /* Client */
DO:
  IF SELF:CHECKED AND SELF:SENSITIVE THEN
    ENABLE profdesc outpfilename DIRECTORY filter-fillin
    WITH FRAME {&FRAME-NAME}.
  ELSE
    DISABLE profdesc outpfilename DIRECTORY filter-fillin
      WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ckServer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ckServer C-Win
ON VALUE-CHANGED OF ckServer IN FRAME DEFAULT-FRAME /* Application Server */
DO:

  IF SELF:CHECKED AND SELF:SENSITIVE THEN
    ENABLE profdesc-2 outpfilename-2 directory-2 filter-fillin-2
    WITH FRAME {&FRAME-NAME}.
  ELSE
    DISABLE profdesc-2 outpfilename-2 directory-2 filter-fillin-2
      WITH FRAME {&FRAME-NAME}.

  IF lServerDisabled THEN
    SELF:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME coverage-tgl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL coverage-tgl C-Win
ON VALUE-CHANGED OF coverage-tgl IN FRAME DEFAULT-FRAME /* Coverage */
DO:
  assign coverage-tgl.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME directory-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL directory-2 C-Win
ON LEAVE OF directory-2 IN FRAME DEFAULT-FRAME
DO:
  assign directory.
  if directory eq ? OR directory = "" then
      directory = ".".
  profiler:directory = directory.
  display profiler:directory @ directory with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filesbtn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filesbtn C-Win
ON CHOOSE OF filesbtn IN FRAME DEFAULT-FRAME /* ... */
DO:
  def var outpfil as char no-undo.
  def var retstat as logical no-undo.
  def var oldval as char no-undo.

  outpfil = profiler:file-name.

  system-dialog get-file outpfil
         title "Profile Data File"
         filters "Profile files (*.out)" "*.out",
                 "All files (*.*)" "*.*"
         initial-dir "."
         create-test-file
         return-to-start-dir
         update retstat.
         
  if retstat then do:
     oldval = profiler:file-name.
     outpfilename = outpfil.
     if outpfilename eq ? OR outpfilename = "" then
        outpfilename = oldval.
     profiler:file-name = outpfilename.
     display profiler:file-name @ outpfilename with frame {&FRAME-NAME}.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filesbtn-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filesbtn-2 C-Win
ON CHOOSE OF filesbtn-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  def var outpfil as char no-undo.
  def var retstat as logical no-undo.
  def var oldval as char no-undo.

  outpfil = outpfilename-2.

  system-dialog get-file outpfil
         title "Profile Data File"
         filters "Profile files (*.out)" "*.out",
                 "All files (*.*)" "*.*"
         initial-dir "."
         create-test-file
         return-to-start-dir
         update retstat.
         
  outpfilename-2 = outpfil.

  DISPLAY outpfilename-2 WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filter-fillin-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filter-fillin-2 C-Win
ON LEAVE OF filter-fillin-2 IN FRAME DEFAULT-FRAME
DO:
  assign filter-fillin.
  if filter-fillin eq ? then
    filter-fillin eq "".
  if filter-fillin eq "" then
     tracing-tgl = no.
  profiler:trace-filter = filter-fillin.
  display tracing-tgl 
          profiler:trace-filter @ filter-fillin with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME listing-tgl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL listing-tgl C-Win
ON VALUE-CHANGED OF listing-tgl IN FRAME DEFAULT-FRAME /* Listings */
DO:
  assign listing-tgl.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME outpfilename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL outpfilename C-Win
ON ENTRY OF outpfilename IN FRAME DEFAULT-FRAME
DO:
  filesbtn:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL outpfilename C-Win
ON LEAVE OF outpfilename IN FRAME DEFAULT-FRAME
DO:
  filesbtn:MOVE-TO-BOTTOM().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME outpfilename-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL outpfilename-2 C-Win
ON ENTRY OF outpfilename-2 IN FRAME DEFAULT-FRAME
DO:
  filesbtn:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL outpfilename-2 C-Win
ON LEAVE OF outpfilename-2 IN FRAME DEFAULT-FRAME
DO:
  def var oldval as char no-undo.         
     oldval = profiler:file-name.
  assign outpfilename.
  if outpfilename eq ? OR outpfilename = "" then
    outpfilename = oldval.
  profiler:file-name = outpfilename.
  display profiler:file-name @ outpfilename with frame {&FRAME-NAME}.
    filesbtn:MOVE-TO-BOTTOM().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME profdesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL profdesc C-Win
ON LEAVE OF profdesc IN FRAME DEFAULT-FRAME
DO:
  ASSIGN profdesc.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME profdesc-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL profdesc-2 C-Win
ON LEAVE OF profdesc-2 IN FRAME DEFAULT-FRAME
DO:
  def var oldval as char no-undo.
  oldval = profiler:description.
  assign profdesc.
  if profdesc eq ? OR profdesc = "" then
    profdesc = oldval.
  profiler:description = profdesc.
  display profiler:description @ profdesc with frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME raProfile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL raProfile C-Win
ON VALUE-CHANGED OF raProfile IN FRAME DEFAULT-FRAME
DO:
  RUN assignFieldValues.

  IF raProfile = "Profile" THEN DO:
    ASSIGN
      listing-tgl:SENSITIVE  = TRUE
      coverage-tgl:SENSITIVE = TRUE
      tracing-tgl:SENSITIVE  = TRUE
      ckClient:SENSITIVE     = TRUE
      ckserver:SENSITIVE     = TRUE
      PROFILER:ENABLED       = TRUE
      .
  END.

  IF raProfile = "Debug" THEN DO:
    ASSIGN
      listing-tgl:SENSITIVE  = FALSE
      coverage-tgl:SENSITIVE = FALSE
      tracing-tgl:SENSITIVE  = FALSE
      ckClient:SENSITIVE     = FALSE
      ckserver:SENSITIVE     = FALSE
      PROFILER:ENABLED       = FALSE
      .

  END.

  IF raProfile = "run" THEN DO:
    ASSIGN
      listing-tgl:SENSITIVE  = FALSE
      coverage-tgl:SENSITIVE = FALSE
      tracing-tgl:SENSITIVE  = FALSE
      ckClient:SENSITIVE     = FALSE
      ckserver:SENSITIVE     = FALSE
      PROFILER:ENABLED       = FALSE
      .
  END.

  APPLY "value-changed" TO ckClient.
  APPLY "value-changed" TO ckServer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME raRunType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL raRunType C-Win
ON VALUE-CHANGED OF raRunType IN FRAME DEFAULT-FRAME
DO:
  IF SELF:SCREEN-VALUE = "&Procedure" THEN
    ASSIGN 
      btnProfile:LABEL = "&Run"
      fiRunCode:LABEL  = "&Procedure".
  ELSE
    ASSIGN 
      btnProfile:LABEL = "&Launch"
      fiRunCode:LABEL  = "&Object".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toClear C-Win
ON VALUE-CHANGED OF toClear IN FRAME DEFAULT-FRAME /* Clear Profiles */
DO:
  IF SELF:CHECKED THEN
    ASSIGN 
      toLoad:SENSITIVE = FALSE
      toLoad:CHECKED   = TRUE.
  ELSE
    toLoad:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tracing-tgl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tracing-tgl C-Win
ON VALUE-CHANGED OF Tracing-tgl IN FRAME DEFAULT-FRAME /* Tracing */
DO:
  assign tracing-tgl.

     
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

  RUN setProfilerEnabled.

  RUN InitializeWindow.

  PROFILE-BLOCK:
  DO ON ERROR UNDO PROFILE-BLOCK, RETRY:
    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
      RUN ProfilerOff NO-ERROR.

      RUN CloseViewerWindow NO-ERROR.
    END.
  END.
END.



procedure ADEPersistent:
return "OK".
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignFieldValues C-Win 
PROCEDURE assignFieldValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    raProfile raruntype firuncode listing-tgl coverage-tgl tracing-tgl
    ckclient ckserver profdesc profdesc-2 outpfilename outpfilename-2
    DIRECTORY directory-2 filter-fillin filter-fillin-2 
    toClear toload ckAutoWrite ckClearCache.

  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckInput C-Win 
PROCEDURE CheckInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF fiRunCode = "" THEN
  RETURN ERROR "Please provide an object to run.".

IF profdesc = "" THEN
  RETURN ERROR "Please provide a description for the client.".

IF profdesc-2 = "" AND ckServer THEN
  RETURN ERROR "Please provide a description for the server.".
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseViewerWindow C-Win 
PROCEDURE CloseViewerWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(hViewer) THEN DO:
    RUN destroyObject IN hViewer NO-ERROR.
  END.

  IF VALID-HANDLE(hViewer) THEN DO:
    DELETE OBJECT hViewer NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebugCode C-Win 
PROCEDURE DebugCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRunContainerType AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hRunContainer     AS HANDLE     NO-UNDO.

DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().
DEBUGGER:VISIBLE = TRUE.

CASE raRunType:
  WHEN "&Procedure":U THEN DO:
    RUN VALUE(fiRunCode).
    RUN StopContainer.
  END.
  WHEN "&Container":U THEN DO:
    RUN launchContainer IN gshSessionManager 
                        (INPUT  fiRunCode            /* object filename if physical/logical names unknown */
                        ,INPUT  "":U                 /* physical object name (with path and extension) if known */
                        ,INPUT  fiRunCode            /* logical object name if applicable and known */
                        ,INPUT  FALSE                 /* run once only flag YES/NO */
                        ,INPUT  "":U                 /* instance attributes to pass to container */
                        ,INPUT  ''                   /* child data key if applicable */
                        ,INPUT  ''                   /* run attribute if required to post into container run */
                        ,INPUT  "":U                 /* container mode, e.g. modify, view, add or copy */
                        ,INPUT  ?                    /* parent (caller) window handle if known (container window handle) */
                        ,INPUT  ?                    /* parent (caller) procedure handle if known (container procedure handle) */
                        ,INPUT  ?                    /* parent (caller) object handle if known (handle at end of toolbar link, e.g. browser) */
                        ,OUTPUT hRunContainer        /* procedure handle of object run/running */
                        ,OUTPUT cRunContainerType    /* procedure type (e.g ADM1, Astra1, ADM2, ICF, "") */
                        ).
    IF RETURN-VALUE > "" THEN
      MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF VALID-HANDLE(hRunContainer) THEN DO:
      CREATE ttRunObject.
      ASSIGN 
        ttRunObject.ObjectHandle = hRunContainer.
    END.

  END.
  OTHERWISE 
    MESSAGE 'programmer error: wrong run type: ' raruntype
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END CASE.

IF VALID-HANDLE(hRunContainer) THEN
  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Launch Time: " + STRING(ETIME / 1000)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableServerProf C-Win 
PROCEDURE disableServerProf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN 
    lServerDisabled = TRUE.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      ckserver:SENSITIVE        = FALSE
      ckserver:CHECKED          = FALSE
      profdesc-2:SENSITIVE      = FALSE
      outpfilename-2:SENSITIVE  = FALSE
      filter-fillin-2:SENSITIVE = FALSE
      filesbtn-2:SENSITIVE      = TRUE.
    
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields C-Win 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

    ENABLE firuncode btnprofile btnstop raProfile listing-tgl
      coverage-tgl tracing-tgl ckautowrite toclear toload
      btnwrite buview filesbtn filesbtn-2.

    APPLY "value-changed" TO raProfile.
    APPLY "value-changed" TO ckClient.
    APPLY "value-changed" TO ckServer.


    IF NOT VALID-HANDLE(gshSessionManager) THEN
      ASSIGN
        raRunType:SENSITIVE    = FALSE
        ckClearCache:SENSITIVE = FALSE.
    ELSE
      ASSIGN
        raRunType:SENSITIVE    = TRUE
        ckClearCache:SENSITIVE = TRUE.


    IF NOT VALID-HANDLE(gshAstraAppserver) OR gshAstraAppServer = SESSION THEN DO:
      RUN disableServerProf.
    END.

    APPLY "entry" TO raRunType.
    
  END.

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
  DISPLAY raProfile raRunType fiRunCode ckClearCache listing-tgl coverage-tgl 
          Tracing-tgl ckClient profdesc outpfilename directory filter-fillin 
          ckServer profdesc-2 outpfilename-2 directory-2 filter-fillin-2 
          ckAutoWrite toClear ToLoad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE filesbtn filesbtn-2 raProfile raRunType fiRunCode ckClearCache 
         btnProfile btnStop listing-tgl coverage-tgl Tracing-tgl ckClient 
         profdesc outpfilename directory filter-fillin ckServer profdesc-2 
         outpfilename-2 directory-2 filter-fillin-2 ckAutoWrite toClear ToLoad 
         btnWrite buview RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchServerProf C-Win 
PROCEDURE fetchServerProf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lProfilerEnabled AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lProfile         AS LOGICAL    NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN ckserver ckAutoWrite.
    lprofile = raProfile = "Profile".
  END.

  IF ckServer AND 
    VALID-HANDLE(gshAstraAppserver) AND
    NOT (gshAstraAppserver = SESSION) THEN DO:
        
    RUN profiler/profileronoff.p ON gshAstraAppserver
      (INPUT TRUE,
       INPUT-OUTPUT lProfile,
       INPUT-OUTPUT lProfilerEnabled,       /* turn off profiler */
       INPUT-OUTPUT listing-tgl,
       INPUT-OUTPUT coverage-tgl,
       INPUT-OUTPUT profdesc-2,
       INPUT-OUTPUT outpfilename-2,
       INPUT-OUTPUT directory-2,
       INPUT-OUTPUT filter-fillin-2,
       INPUT        ckAutoWrite) NO-ERROR.

     IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "Could not configure profiler on appserver" 
         RETURN-VALUE ERROR-STATUS:GET-MESSAGE(1)
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

       RUN disableServerProf.
     END.
  END.
  ELSE DO:
    RUN disableServerProf.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchServerProfData C-Win 
PROCEDURE fetchServerProfData :
/*------------------------------------------------------------------------------
  Purpose:     fetch the profile output file from the appserver
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE mFile AS MEMPTR     NO-UNDO.

IF ckServer:CHECKED IN FRAME {&FRAME-NAME}= TRUE THEN DO:
  RUN profiler/profgetfile.p ON gshAstraAppserver
    (OUTPUT mFile) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Could not fetch server profile from appserver because' skip
            'I could not find the file: profiler/profgetfile.p'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  IF GET-SIZE(mFile) > 0 THEN DO:
    OUTPUT STREAM sFile TO VALUE(outpfilename-2) BINARY NO-MAP NO-CONVERT.
    EXPORT STREAM sFile mFile.
    OUTPUT STREAM sFile CLOSE.
    SET-SIZE(mFile) = 0.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeWindow C-Win 
PROCEDURE InitializeWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEFINE VARIABLE lProfilerEnabled AS LOGICAL    NO-UNDO INIT FALSE.

  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "PROFILER_MESSAGE" ANYWHERE.
  STATUS INPUT OFF.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      listing-tgl:CHECKED  = FALSE
      coverage-tgl:CHECKED = FALSE
      PROFILER:PROFILING   = FALSE
      tracing-tgl:CHECKED  = FALSE
      PROFILER:LISTINGS    = FALSE
      PROFILER:FILE-NAME   = "clntprof" + 
           STRING (YEAR(today)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY))+ ".out"
      PROFILER:DIRECTORY   = SESSION:TEMP-DIR
      PROFILER:DESCRIPTION = "Client Profile " + STRING(TODAY, "99-99-9999")
      profdesc-2           = "Server Profile " + STRING(TODAY, "99-99-9999")
      outpfilename-2       = "srvrprof" +
        STRING (YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY))+ ".out"
  
      .
  
    IF directory-2 = "" THEN
      directory-2 = SESSION:TEMP-DIR.
  
    /* fetch server side settings */
  
    RUN fetchServerProf.
  
    IF PROFILER:ENABLED THEN
      raProfile = "Profile".
    ELSE
      raProfile = "Run".

    ASSIGN listing-tgl   = profiler:listings
           coverage-tgl  = profiler:coverage
           directory     = profiler:directory
           tracing-tgl   = if profiler:trace-filter EQ "" then no else yes
           filter-fillin = profiler:trace-filter
           outpfilename  = profiler:file-name
           profdesc      = profiler:description
           .
  
  
    DISPLAY raProfile listing-tgl coverage-tgl DIRECTORY tracing-tgl filter-fillin
      outpfilename profdesc directory-2 tracing-tgl filter-fillin-2 outpfilename-2
      profdesc-2 WITH FRAME {&FRAME-NAME}.
  
    RUN enableFields.
    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProfileCode C-Win 
PROCEDURE ProfileCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRunContainerType  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hRepositoryManager AS HANDLE     NO-UNDO.
DEFINE VARIABLE lEnableProfiler    AS LOGICAL    NO-UNDO INIT TRUE.
DEFINE VARIABLE lProfileCode       AS LOGICAL    NO-UNDO.

IF ERROR-STATUS:ERROR OR RETURN-VALUE > "" THEN
  RETURN RETURN-VALUE.

RUN setProfilerOptions.

IF VALID-HANDLE(gshSessionManager) THEN DO:
  IF ckClearCache THEN

    hRepositoryManager = DYNAMIC-FUNCTION("getManagerHandle" in gshSessionManager,
                                          INPUT "RepositoryManager").
  IF VALID-HANDLE(hRepositoryManager) THEN DO:
    RUN clearClientCache IN hRepositoryManager.
    RUN destroyClassCache IN hRepositoryManager.
  END.
END.

lProfileCode = raProfile = "Profile".

IF ckServer AND 
  VALID-HANDLE(gshAstraAppserver) AND
  /* make sure the appserver is not pointed to the local session */
  NOT (gshAstraAppserver = SESSION) THEN DO:
  /* turn the the profiler on for the server */
  RUN profiler/profileronoff.p ON gshAstraAppserver
    (INPUT TRUE,
     INPUT-OUTPUT lprofileCode,
     INPUT-OUTPUT lEnableProfiler,
     INPUT-OUTPUT listing-tgl,
     INPUT-OUTPUT coverage-tgl,
     INPUT-OUTPUT profdesc-2,
     INPUT-OUTPUT outpfilename-2,
     INPUT-OUTPUT directory-2,
     INPUT-OUTPUT filter-fillin-2,
     INPUT ckAutoWrite) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Could not profile server because I could not find' SKIP
            'the file on the appserver: profiler/profileronoff.p'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END.
    
PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Launching Container: " + firuncode).

/* don't bother checking for sensitivity because the
   radio-set enables/disables the profiler */
IF ckClient THEN
  PROFILER:PROFILING = TRUE.

ETIME(YES).

CASE raRunType:
  WHEN "&Procedure":U THEN DO:
    RUN VALUE(fiRunCode).
    RUN StopContainer.
  END.
  WHEN "&Container":U THEN DO:
    RUN launchContainer IN gshSessionManager 
                        (INPUT  fiRunCode            /* object filename if physical/logical names unknown */
                        ,INPUT  "":U                 /* physical object name (with path and extension) if known */
                        ,INPUT  fiRunCode            /* logical object name if applicable and known */
                        ,INPUT  FALSE                 /* run once only flag YES/NO */
                        ,INPUT  "":U                 /* instance attributes to pass to container */
                        ,INPUT  ''                   /* child data key if applicable */
                        ,INPUT  ''                   /* run attribute if required to post into container run */
                        ,INPUT  "":U                 /* container mode, e.g. modify, view, add or copy */
                        ,INPUT  ?                    /* parent (caller) window handle if known (container window handle) */
                        ,INPUT  ?                    /* parent (caller) procedure handle if known (container procedure handle) */
                        ,INPUT  ?                    /* parent (caller) object handle if known (handle at end of toolbar link, e.g. browser) */
                        ,OUTPUT hRunContainer        /* procedure handle of object run/running */
                        ,OUTPUT cRunContainerType    /* procedure type (e.g ADM1, Astra1, ADM2, ICF, "") */
                        ).
    IF RETURN-VALUE > "" THEN
      MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF VALID-HANDLE(hRunContainer) THEN DO:
      CREATE ttRunObject.
      ASSIGN 
        ttRunObject.ObjectHandle = hRunContainer.
    END.

  END.
  OTHERWISE 
    MESSAGE 'programmer error: wrong run type: ' raruntype
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END CASE.

IF VALID-HANDLE(hRunContainer) THEN
  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Launch Time: " + STRING(ETIME / 1000)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProfilerOff C-Win 
PROCEDURE ProfilerOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
    PROFILER:PROFILING = FALSE
    PROFILER:ENABLED   = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE profiler_message C-Win 
PROCEDURE profiler_message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcMessage AS CHARACTER  NO-UNDO.

PROCESS EVENTS.

  STATUS DEFAULT pcMessage IN WINDOW {&WINDOW-NAME}:HANDLE.

PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProfFields C-Win 
PROCEDURE setProfFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE lEnabled AS LOGICAL    NO-UNDO.

lEnabled = PROFILER:ENABLED.


DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    ckClient:SENSITIVE = lenabled
    ckserver:SENSITIVE = lenabled
    .
  IF NOT VALID-HANDLE(gshAstraAppserver) OR gshAstraAppServer = SESSION THEN DO:
    RUN disableServerProf.
  END.
  
  APPLY "value-changed" TO ckClient.
  APPLY "value-changed" TO ckserver.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProfilerEnabled C-Win 
PROCEDURE setProfilerEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lanswer AS LOGICAL    NO-UNDO.
  IF NOT PROFILER:ENABLED THEN DO:
    MESSAGE "The profiler is currently disabled." SKIP
      "Would you like to enable it?"
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO SET lanswer.
  
  END.
  
  DO WITH FRAME {&FRAME-NAME}:

    PROFILER:ENABLED = lAnswer.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProfilerOptions C-Win 
PROCEDURE setProfilerOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    PROFILER:LISTINGS    = listing-tgl
    PROFILER:COVERAGE    = coverage-tgl
    PROFILER:DESCRIPTION = profdesc
    PROFILER:FILE-NAME   = outpfilename 
    PROFILER:DIRECTORY   = DIRECTORY
    .
    IF NOT tracing-tgl then
       PROFILER:TRACE-FILTER = "".
    ELSE IF PROFILER:TRACE-FILTER EQ "" then
       PROFILER:TRACE-FILTER = "*".

    /* make sure we don't overwrite anything */

    DISPLAY filter-fillin.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StopContainer C-Win 
PROCEDURE StopContainer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE lProfilerEnabled AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lProfileCode     AS LOGICAL    NO-UNDO.

  DEBUGGER:CLEAR().

  ASSIGN 
    PROFILER:PROFILING = FALSE.

  RUN assignFieldValues.

  IF raProfile = "Profile" THEN
    lProfilecode = TRUE.
  ELSE
    lProfileCode = FALSE.

  IF ckServer AND VALID-HANDLE(gshAstraAppserver) AND
    NOT (gshAstraAppserver = SESSION) THEN DO:
    RUN profiler/profileronoff.p ON gshAstraAppserver
      (INPUT TRUE,
       INPUT-OUTPUT lProfileCode,
       INPUT-OUTPUT lProfilerEnabled,       /* turn off profiler */
       INPUT-OUTPUT listing-tgl,
       INPUT-OUTPUT coverage-tgl,
       INPUT-OUTPUT profdesc-2,
       INPUT-OUTPUT outpfilename-2,
       INPUT-OUTPUT directory-2,
       INPUT-OUTPUT filter-fillin-2,
       INPUT        ckAutoWrite) NO-ERROR.

  END.

  FOR EACH ttRunObject:
    IF VALID-HANDLE( ttRunObject.ObjectHandle) THEN DO:
      IF VALID-HANDLE(ttRunObject.ObjectHandle) THEN
        APPLY "close" TO ttRunObject.ObjectHandle.

      IF VALID-HANDLE(ttRunObject.ObjectHandle) THEN 
        RUN destroyObject IN ttRunObject.ObjectHandle NO-ERROR.

      IF VALID-HANDLE(ttRunObject.ObjectHandle) THEN
        DELETE OBJECT ttRunObject.ObjectHandle NO-ERROR.

    END.

  END.


  IF ckAutoWrite THEN
    RUN writeProfileData.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewProfile C-Win 
PROCEDURE viewProfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF toClear THEN DO:
    EMPTY TEMP-TABLE ttSource .
    EMPTY TEMP-TABLE calltree.
    EMPTY TEMP-TABLE sumstmt.
    EMPTY TEMP-TABLE profile-session.
    EMPTY TEMP-TABLE calltreedata.
    EMPTY TEMP-TABLE listing-lines.
    EMPTY TEMP-TABLE executable-line.
    EMPTY TEMP-TABLE traceinfo.
    EMPTY TEMP-TABLE USER-DATA.

  END.

  IF toLoad THEN DO:
    iSessionId = iSessionId + 1.

    RUN PROFILER/importprof.p (INPUT iSessionId,
                               INPUT outpfilename,
                               INPUT profdesc,
                               OUTPUT TABLE ttSOURCE APPEND,
                               OUTPUT TABLE calltree APPEND,
                               OUTPUT TABLE sumstmt APPEND,
                               OUTPUT TABLE profile-session APPEND,
                               OUTPUT TABLE calltreedata APPEND,
                               OUTPUT TABLE listing-lines APPEND,
                               OUTPUT TABLE executable-line APPEND,
                               OUTPUT TABLE traceinfo APPEND,
                               OUTPUT TABLE USER-DATA APPEND) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE RETURN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    IF SEARCH(outpfilename-2) <> ? THEN DO:
      iSessionId = iSessionId + 1.
      RUN PROFILER/importprof.p (INPUT iSessionId,
                                 INPUT outpfilename-2,
                                 INPUT profdesc-2,
                                 OUTPUT TABLE ttSOURCE APPEND,
                                 OUTPUT TABLE calltree APPEND,
                                 OUTPUT TABLE sumstmt APPEND,
                                 OUTPUT TABLE profile-session APPEND,
                                 OUTPUT TABLE calltreedata APPEND,
                                 OUTPUT TABLE listing-lines APPEND,
                                 OUTPUT TABLE executable-line APPEND,
                                 OUTPUT TABLE traceinfo APPEND,
                                 OUTPUT TABLE USER-DATA APPEND) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.

    END.
  END.

  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Loading Profile Viewer").

  IF CAN-FIND (FIRST profile-session) THEN
    RUN PROFILER/viewprofilew.w PERSISTENT SET hViewer.
  ELSE DO:
    MESSAGE 'There are no profiles loaded.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  IF valid-handle(hViewer) THEN DO:
    RUN initializeObject IN hViewer.
    RUN ENABLE_ui IN hViewer.
    RUN changeSession IN hViewer (INPUT iSessionId) .
  END.

  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewProfileData C-Win 
PROCEDURE ViewProfileData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN assignFieldValues.

DO WITH FRAME {&FRAME-NAME}:

  DISABLE
    ALL WITH FRAME {&FRAME-NAME}.

  SESSION:SET-WAIT-STATE("general":u).

  RUN viewProfile NO-ERROR.

  ASSIGN
    toClear:CHECKED  = FALSE.

  SESSION:SET-WAIT-STATE("":u).

  RUN enableFields.
  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeProfileData C-Win 
PROCEDURE writeProfileData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wrote_it as logical no-undo.
  def var msg as char no-undo.
  def var i as int no-undo.
  
  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Writing Profile Data").

  run adecomm/_setcurs.p ("WAIT").

  wrote_it = profiler:write-data() no-error.
  
  run adecomm/_setcurs.p ("").
  
  RUN fetchServerProfData.

  PUBLISH "PROFILER_MESSAGE" FROM THIS-PROCEDURE (INPUT "Profile Data Written to: " + PROFILER:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


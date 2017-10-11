  DEFINE {1} {2} TEMP-TABLE ttSource NO-UNDO RCODE-INFO
    FIELD srcid            AS integer 
    FIELD srcname          AS character FORMAT "X(150)" COLUMN-LABEL "Code Block"
    FIELD srcfile          AS CHARACTER
    FIELD xreffile         AS CHARACTER
    FIELD parent           AS integer 
    FIELD avg_acttime      AS decimal FORMAT ">>9.999999" COLUMN-LABEL "Avg Time"
    FIELD tot_acttime      AS decimal 
    FIELD tot_parenttime   AS decimal 
    FIELD tot_cumtime      AS DECIMAL FORMAT ">>>>,>>>9.999999" COLUMN-LABEL "Cum Time"
    FIELD listname         AS CHARACTER 
    FIELD session-id       AS integer 
    FIELD callcnt          AS integer FORMAT ">>9.999999" COLUMN-LABEL "Calls To"
    FIELD session-percent  AS decimal FORMAT ">>9.999999" COLUMN-LABEL "% Session"
    FIELD percall-percent  AS decimal 
    FIELD overhead_time    AS decimal 
    FIELD first-line       AS integer
    FIELD total-time       AS DECIMAL FORMAT ">,>>9.999999"     COLUMN-LABEL "Tot Time"
    FIELD srcexectime      AS DECIMAL  /* total executation time for this procedure and its children */
    FIELD CRC-Val          AS integer 
    INDEX sourceid AS PRIMARY UNIQUE session-id srcid
    INDEX sessparent session-id  parent tot_acttime DESCENDING
    INDEX tot-actualtime  session-id  tot_acttime DESCENDING .

  DEFINE {1} {2} TEMP-TABLE calltree NO-UNDO RCODE-INFO
    FIELD caller AS integer 
    FIELD callee AS integer 
    FIELD callcnt AS integer 
    FIELD session-id AS integer 
    INDEX caller-callee as UNIQUE PRIMARY session-id  caller  callee 
    INDEX callee session-id  callee  .

  
  DEFINE {1} {2} TEMP-TABLE sumstmt NO-UNDO RCODE-INFO
    FIELD stmtcnt               AS integer COLUMN-LABEL "Exec Count" FORMAT ">>>,>>9"
    FIELD acttime               AS decimal COLUMN-LABEL "Avg Exec"   FORMAT ">,>>9.999999"
    FIELD lineno                AS integer COLUMN-LABEL "Line" FORMAT ">>>,>>9"
    FIELD srcid                 AS integer 
    FIELD cumtime               AS decimal
    FIELD tot_cumtime           AS DECIMAL COLUMN-LABEL "Cum Time"   FORMAT ">,>>>9.999999"
    FIELD tot_acttime           AS decimal COLUMN-LABEL "Tot Time"   FORMAT  ">,>>9.999999"
    FIELD session-id            AS integer 
    FIELD session-percent       AS decimal 
    FIELD perprocedure-percent  AS decimal 
    FIELD parent  AS integer 
    INDEX actual-time AS PRIMARY session-id acttime
    index cumulative-time session-id  cumtime DESCENDING 
    index parent  session-id  parent  lineno  
    index src-line  AS UNIQUE session-id  srcid  lineno  
    index tottime  session-id  srcid  tot_acttime  .

  DEFINE {1} {2} TEMP-TABLE Profile-Session NO-UNDO RCODE-INFO
    field session-id  AS integer 
    field Session-Desc  AS character 
    field session-date  AS date 
    field tot_acttime  AS decimal 
    field session-notes  AS character 
    field Session-Time  AS character 
    field Session-User  AS character 
    index sessid  AS UNIQUE PRIMARY session-id .

  DEFINE {1} {2} TEMP-TABLE CallTreeData NO-UNDO RCODE-INFO
    field caller  AS integer 
    field callee  AS integer 
    field callcnt  AS integer 
    field session-id  AS integer 
    field callerlineno  AS integer 
    index caller-callee AS PRIMARY session-id   caller  callerlineno  callee  
    index callee session-id  callee  .

  DEFINE {1} {2} TEMP-TABLE Listing-Lines NO-UNDO RCODE-INFO
    field session-id  AS integer 
    field srcid  AS integer 
    field line-no  AS integer 
    field line-text  AS character 
    field avg-acttime  AS decimal 
    field stmtcnt  AS integer 
    index src-line AS UNIQUE PRIMARY session-id  srcid  line-no  .

  DEFINE {1} {2} TEMP-TABLE Executable-Line NO-UNDO RCODE-INFO
    field session-id  AS integer 
    field srcid  AS integer 
    field line-no  AS integer 
    index exec-line  AS PRIMARY session-id  srcid  line-no  .

  DEFINE {1} {2} TEMP-TABLE Traceinfo NO-UNDO RCODE-INFO
    field session-id  AS integer 
    field srcid  AS integer 
    field line-no  AS integer 
    field acttime  AS decimal 
    field starttime  AS decimal 
    index tracing  AS PRIMARY session-id  starttime  .

  DEFINE {1} {2} TEMP-TABLE User-Data NO-UNDO RCODE-INFO
    field session-id  AS integer 
    field event-time  AS decimal 
    field Data  AS character 
    index user-profiling  AS PRIMARY session-id  event-time  .

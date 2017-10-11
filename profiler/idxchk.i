DEFINE TEMP-TABLE ttIndex NO-UNDO
    FIELD sessionid    AS INTEGER
    FIELD srcid        AS INTEGER
    FIELD LineId       AS INT
    FIELD DatabaseName AS CHAR
    FIELD TableName    AS CHAR
    FIELD IndexName    AS CHAR
    FIELD FieldName    AS CHAR
    FIELD FieldSeq     AS INT
    FIELD SortOrder    AS CHAR    /* A or D */
    FIELD IsUnique     AS LOG
    INDEX i1 IS PRIMARY UNIQUE sessionid srcid LineId
    INDEX i2 srcid DatabaseName TableName IndexName FieldSeq.

/* Temptable storing the SEARCH lines from the XRef file */
DEFINE TEMP-TABLE ttSearch NO-UNDO
    FIELD sessionid      AS INTEGER
    FIELD srcid          AS INTEGER
    FIELD LineId         AS INT
    FIELD Xref-LineId    AS INT
    FIELD SearchType     AS CHAR    /* Can-Find, Find, For-Each */
    FIELD DatabaseName   AS CHAR
    FIELD TableName      AS CHAR
    FIELD UsedIndexes    AS CHAR    /* Comma-separated list */
    FIELD AccessedFields AS CHAR    /* Comma-separated list */
    FIELD IndexScore1    AS DEC 
    FIELD IndexScore2    AS DEC 
    INDEX i1 IS PRIMARY UNIQUE sessionid srcid LineId
    INDEX i2 IndexScore1            /* For Browse Sort */
    INDEX i3 IndexScore2.           /* For Browse Sort */

/* Temptable storing the lines from XRef file */
DEF TEMP-TABLE ttXRef NO-UNDO
    FIELD sessionid      AS INTEGER
    FIELD srcid          AS INTEGER
    FIELD LineId         AS INT
    FIELD xProcName      AS CHAR
    FIELD xFileName      AS CHAR
    FIELD xLineNumber    AS INT
    FIELD xRefType       AS CHAR
    FIELD xObjectId      AS CHAR
    FIELD xMisc1         AS CHAR
    FIELD xMisc2         AS CHAR
    FIELD xMisc3         AS CHAR
    FIELD xMisc4         AS CHAR
    FIELD xMisc5         AS CHAR
    INDEX i1 IS PRIMARY UNIQUE sessionid srcid LineId
    INDEX i2 srcid xProcName xFileName xLineNumber.

/* Temptable storing the Temptable structure */
DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD sessionid      AS INTEGER
    FIELD srcid          AS INTEGER
    FIELD LineId         AS INT
    FIELD TTName         AS CHAR
    FIELD TTDefinition   AS CHAR
    FIELD TTDBReference  AS CHAR
    INDEX i1 IS PRIMARY UNIQUE sessionid srcid LineId
    INDEX i2 TTName
  .

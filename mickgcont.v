/* mket/mick/mickgcont.v - Marketing get the contact details 

30/11/2017 prhs - WS12290 - created
--------------------------------------------------------------------------- */
/*---------------------------------------------------------------------------
NOTE - the datain-tt is a throwaway single record table to avoid too many
       input parameters being define and allow for future changes without
       disrupting the whole system

       mch-proc-i   -  mketcomh.mch-proc        (obligatory)

       There MUST be one of either the mcserial-i or contract-r fields.
       If both are supplied then the contract company will override
       any marketing company mc serial passed in.

       ml-link-i if this is passed in overrides the mketcomh link id

       POL - contract-r, tt-key-c, and let-type-c are obligatory for:
                                 mch-proc-i
               Recalls           - 39
               Service reminders - 40
               MOT reminders     - 41
               VED renewals      - 65
----------------------------------------------------------------------------*/
define {&new} shared temp-table datain-tt no-undo
    field mch-proc-i as integer       /* mketcomh process      (dsetomth.d) */
    field ml-link-i  as integer       /* link system area id   (dsetsyco.d) */
    field mcserial-i as integer       /* mketcomp serial       (optional)   */
    field mketdept-c as character     /* department            (optional)   */
    field mketlocn-c as character     /* location              (optional)   */
    field contract-r as rowid         /* contract rowid        (optional)   */
    field sldebit2-r as rowid         /* SL DD    rowid        (optional)   */
    field sl-acc-c   as character     /* SL CredCon letters    (optopnal)   */
    field tt-key-c   as character     /* unique key for the contact-tt      */
    field POL-lets-l as logical       /* to validate POL letter data        */
    field let-type-c as character     /* POL - letter type e.g. MOT1,2 3 4  */
    field override-c as character     /* POL - VED set from vehrftax field  */
    field event-c    as character     /* POL - MOT1/2 VED1 etc.             */
    field empty-tt-l as logical       /* tell mickgcont to empty contact-tt */
    index a is primary unique mch-proc-i.
    
/*---------------------------------------------------------------
temp table defs for output of the data - Added to for POL letters
----------------------------------------------------------------*/
define {&new} shared temp-table contact-tt no-undo
    field fullname-c  as character    /* letters sms mobile number, contact
                                         name or whatever                   */
    field inits-c     as character    /* contact initials                   */
    field title-c     as character    /* contact title                      */
    field name-c      as character    /* contact name                       */
    field compname-c  as character    /* letter name to be used             */
    field email-c     as character    /* email address to be used           */
    field addresse-c  as character    /* Addresse                           */
    field address-c   as character 
                         extent 7     /* full address including postcode    */

    /*----------------------------------------------------
    Maintain postcode field - required for XGC substitute!
        Needs to be deprecated !
    -----------------------------------------------------*/
    field postcode-c  as character

    field output-i    as integer      /* letters set mketcomm method code   */
    field error-l     as logical      /* NOTE - NOT USED CURRENTLY          */
    field personal-l  as logical      /* Personal contract product type     */
    field fleet-l     as logical      /* Fleet managed mco-fleetman <> 0    */
    field error-c     as character    /* NOT USED CURRENTLY                 */
    field pdf-file-c  as character    /* MOT & VED letters set this         */
    field where-c     as character    /* May hold anything !!               */
    field mobile-c    as character    /* contact mobile number              */
    field mketcont-r  as rowid     
    field mketcont-re as recid     format "zzzzzzz9" /* why both row & recid's*/
    field mketnadd-r  as rowid
    field mketnadd-re as recid     format "zzzzzzz9"
    field mketcomp-r  as rowid
    field mketcomp-re as recid     format "zzzzzzz9"
    field hdbranch-r  as rowid
    field key-c       as character    /* key field passed in via datain-tt  */
    field admin-c     as character    /* NOT USED CURRENTLY                 */
    field comp-addr-l as logical      /* set if using company adddress      */
    /*----------------------------------------------------------------------
    The above mainly for POL letters - the following the rest of the  system
    -----------------------------------------------------------------------*/
    field cc-list-c   as character    /* email copy to (carbon copy)        */
    field bcc-list-c  as character    /* email copy to (Blind copy )        */
    field mch-proc-i  as integer      /* mketcomh process id                */
    field proc-desc-c as character    /* mketcomh description               */
/*-----------------------------------------------
HS - do we need this?  we have output-i already!
------------------------------------------------*/
    field ml-link-i   as integer      /* Comms statcode link (sect MC)      */
    field link-desc-c as character    /* Comms statcode desc (sect MC)      */
    field mcm-code-i  as integer      /* mketcomm method code               */
    field method-c    as character format "x(14)"    /* mketcomm description  
                                                        see output-i above  */
    field mcserial-i  as integer   format "zzzzzzz9" /* mketcomp serial
                                                        used (info only)    */
    field dept-c      as character format "x(12)"
    field locn-c      as character format "x(12)"
    /*----------------------------------------------------------------
    the next for RCAL 'driver direct' fleet manager email copy details 
    Recalls are always printed NOT emailed - so we just send a list of
     the registrations in an email to the Fleet Manager
    -----------------------------------------------------------------*/
    field fm-mcoid-i  as integer   format "zzzzzzz9"

    index key-c is primary unique key-c
    index mlnk  ml-link-i mch-proc-i.
   

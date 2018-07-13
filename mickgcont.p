/* mket/mick/mickgcont.p - Marketing get the contact details

28/11/2017 prhs - WS12290 - created (majority based on xsysgetcont)
--------------------------------------------------------------------------- */
/*-------------------------------------------------------------------------
     Black box to work out who gets what and by what method
              for a specific process area etc.

     Input data is via the temp-table datain-tt as follows:

         mch-proc-i  -  mketcomh.mch-proc            (obligatory)
         ml-link-i   -  link system area             (optional)
         mcserial-i  -  marketing company serial     (optional)
         mketdept-c  -  department                   (optional)
         mketlocn-c  -  location                     (optional)
         contract-r  -  rowid of the contract        (optional)
         sldebit2-r  -  rowid for SL DD notifs       (optional
                                                      obligatory for 32)
         sl-acc-c    -  SL account Cred Con          (optional
                                                      obligatory for 43)
         tt-key-c    -  contact-tt UNIQUE key field  (obligatory)
         ----------------------------------------------------------
         NOTE - for POL letters the following are mainly obligatory
         ----------------------------------------------------------
         POL-lets-l  -  validate POL letter data     (optional/obligatory)
         let-type-c  -  POL letter type reqd         (optional/obligatory)
         override-c  -  POL override destination     (optional)
         event-c     -  POL event type MOT1/2 etc.   (optional/obligatory)

         empty-tt-l  -  logical to drive the 'empty' clearing of the
                        contact-tt temp table.  Default is obviously
                        false which will NOT clear the data.

     Information is output via another temp-table contact-tt which may
     may have multiple records - added to for POL letters etc. hence 
     the 'empty' instruction flag.  
    
NOTE - a) Errors are dealt with by return-value and the calling process
          MUST handle these as appropriate.

       b) Normally there will to be one of either mcserial-i, or contract-r
          except for specific processes.  If both are supplied then the
          contract SL account marketing company will override any mcserial-i
          passed in.

       c) ml-link-i  - if passed in overrides the mketcomh defined link id.

       d) sldebit2-r - obligatory for mch-proc-i 32

       e) sl-acc-c   - if passed in the sl account mc-serial overrides any
                       mcserial-i passed in.

       f) contract-r, tt-key-c, and let-type-c are obligatory for POL
          letter processing (POL-lets-l = true):
                                   mch-proc-i
                 Recalls           - 39
                 Service reminders - 40
                 MOT reminders     - 41
                 VED renewals      - 65

       g) The datain-tt values may be changed based on what we evaluate

--------------------------------------------------------------------------*/
{mket/mick/mickgcont.v} /* input-output shared temp table definitions */

{mket/mick/mickaddr.v}  /* for address packing */
{invs/ginv/ginvbod.v}   /* for mickfcon        */

define variable where-c        as character no-undo.
define variable branch-code-c  as character no-undo.
define variable branch-desc-c  as character no-undo.
define variable branch-email-c as character no-undo.
define variable edi-funct-c    as character no-undo.
define variable letter-c       as character no-undo.
define variable op-addr-c      as character no-undo extent 6.
define variable ip-addr-c      as character no-undo extent 6.
define variable postcode-c     as character no-undo.
define variable tmp-addr-c     as character no-undo.
define variable mob-num-c      as character no-undo.
define variable fm-cc-c        as character no-undo.
define variable pdffile-c      as character no-undo.
define variable use-mcser-i    as integer   no-undo.
define variable mn-ser-i       as integer   no-undo.
define variable letter-i       as integer   no-undo.
define variable let-pdf-i      as integer   no-undo.
define variable loop-i         as integer   no-undo.
define variable head-i         as integer   no-undo.
define variable fm-mco-id-i    as integer   no-undo.
define variable outmeth-i      as integer   no-undo.
define variable use-driver-l   as logical   no-undo.
define variable copy-to-fm-l   as logical   no-undo.
define variable optio-l        as logical   no-undo.
define variable sms-l          as logical   no-undo.
define variable escalate-l     as logical   no-undo.
define variable branch-l       as logical   no-undo.
define variable fleetmanaged-l as logical   no-undo.
define variable comp-addr-l    as logical   no-undo.
define variable mketnadd-r     as recid     no-undo.
define variable contact-tt-r   as rowid     no-undo.
define variable mketlink-r     as rowid     no-undo.
define variable mketcont-r     as rowid     no-undo.

/*---------------------------------------------
Marketing system 'enhanced links' system option
----------------------------------------------*/
{sysm/cap3/cap3mk02.i &var = capt-mk02-l}

define buffer mketcomp#      for mketcomp.
define buffer ddnot-mketbacc for mketbacc.
define buffer ddnot-mketdept for mketdept.

for each datain-tt no-lock:
    assign loop-i     = loop-i + 1.
end.
if loop-i > 1 then
    return error "ERROR - Multiple datain-tt supplied to mickgcont.p".

/*--------------------------------------------------------------
find and Validate the communication header requirements supplied
--------------------------------------------------------------*/
find datain-tt no-lock.
if datain-tt.mch-proc-i = 0 then
    return error "ERROR - mch-proc-i MUST be supplied to mickgcont.p".

find mketcomh where mketcomh.mch-proc = datain-tt.mch-proc-i no-lock no-error.
if not available mketcomh then
    return error "ERROR - Invalid mch-proc-i supplied to mickgcont.p".

/*-----------------------------------------------------------------------
Clear out the contact tt if required - NOTE - POL letters hang on to them
------------------------------------------------------------------------*/
if datain-tt.empty-tt-l then
    empty temp-table contact-tt.

/*----------------------------------------------------------------
ProLease On Line - Check we have the required values passed in for
                   RECALL, SERVICE, MOT, and VED letter processing
-----------------------------------------------------------------*/
if lookup(string(datain-tt.mch-proc-i), "39,40,41,65") > 0
    and datain-tt.POL-lets-l = true then
do:
    find contact-tt
        where contact-tt.key-c = datain-tt.tt-key-c
        no-lock no-error.
    if available contact-tt then
        return error "ERROR - Duplicate tt key value passed to mickgcont.p".

    if datain-tt.contract-r = ? then
        return error "ERROR - Contract rowid must be passed into mickgcont.p".

    if datain-tt.let-type-c = "" then
        return error "ERROR - Letter type must be passed into mickgcont.p".
end.

/*--------------------------------------------------------------------
Check the link id is valid if passed in - otherwise set to Comm header
--------------------------------------------------------------------*/
if datain-tt.ml-link-i <> 0 then
do:
    find statcode 
        where statcode.stat-code = "MC" + string(datain-tt.ml-link-i, "99")
        no-lock no-error.

    if not available statcode then
        return error "ERROR - Invalid Link id passed into mickgcont.p".
end.
else
    assign datain-tt.ml-link-i = mketcomh.ml-link.

/*--------------------------------------------------------------------
Get the Marketing Company record from the mc serial if it's passed in 
 if a contract is passed in check its all valid and use that company
---------------------------------------------------------------------*/
if datain-tt.mcserial-i <> 0 then
    find mketcomp 
        where mketcomp.mc-serial = datain-tt.mcserial-i no-lock no-error.

if datain-tt.contract-r <> ? then 
do:
    find contract where rowid(contract) = datain-tt.contract-r no-lock no-error.
    if not available contract then
        return error "ERROR - invalid contract-r supplied to mickgcont.p".

    find slaccdet
        where slaccdet.sl-account = contract.sl-account no-lock no-error.
    if not available slaccdet then
        return error "ERROR - invalid contract SL account in mickgcont.p".

    find mketcomp 
        where mketcomp.mc-serial = slaccdet.mc-serial no-lock no-error.
    if not available mketcomp then
        return error "ERROR - invalid Company for SL account in mickgcont.p".

    /*-----------------------------------------------------------------
    NOTE - The contract company serial, dept, & locn overrides anything
           that's passed in so reset the input temp table values
    -----------------------------------------------------------------*/
    assign datain-tt.mcserial-i = mketcomp.mc-serial
           datain-tt.mketdept-c = contract.c-dept
           datain-tt.mketlocn-c = contract.c-locn
           datain-tt.sl-acc-c   = slaccdet.sl-account
           .
end.    
/*-----------------
SL DD notifications
------------------*/
if datain-tt.mch-proc = 33 then 
do:
    /*----------------------------------------------------
    NOTE - Do not error if not found - mcon runs this for
           everything and has no sldebit2 rowid to pass in
    -----------------------------------------------------*/
    find sldebit2 
        where rowid(sldebit2) = datain-tt.sldebit2-r 
        no-lock no-error.
end.
/*-----------------------
SL Credit Control Letters
------------------------*/
if datain-tt.mch-proc = 43 then 
do:
    find slaccdet
        where slaccdet.sl-account = datain-tt.sl-acc-c
        no-lock no-error.
    if available slaccdet then
        find mketcomp
            where mketcomp.mc-serial = slaccdet.mc-serial
            no-lock no-error.
end.
/*-----------------------------------------------------------
We should have a marketing company by now - if not then abort
------------------------------------------------------------*/
if not available mketcomp then
    return error "ERROR - Company record not found in mickgcont.p".
/*----------------------------------------------------------------
Now check that any dept/locn value passed in are valid

    NOTE - unless we have an mketdept/locn validation database 
           table ANYTHING at all is valid
-----------------------------------------------------------------*/
if datain-tt.mketdept-c <> "" then
do:
    find mketdept 
        where mketdept.mc-serial = mketcomp.mc-serial
        and   mketdept.mcd-dept  = datain-tt.mketdept-c no-lock no-error.
    if not available mketdept then
    do:
        find first mketdept
            where mketdept.mc-serial = mketcomp.mc-serial
            no-lock no-error.
        if available mketdept then
            return error
                "ERROR - Invalid marketing dept passed into mickgcont.p - "
                         + datain-tt.mketdept-c.
    end.
end.
if datain-tt.mketlocn-c <> "" then
do:
    find mketlocn 
        where mketlocn.mc-serial = mketcomp.mc-serial
        and   mketlocn.mcn-locn  = datain-tt.mketlocn-c no-lock no-error.
    if not available mketlocn then
    do:
        find first mketlocn
            where mketlocn.mc-serial = mketcomp.mc-serial
            no-lock no-error.
        if available mketlocn then
            return error
                "ERROR - Invalid marketing locn passed into mickgcont.p - "
                         + datain-tt.mketlocn-c.
    end.
end.
/*-----------------------------------------------------------------------
If the company is fleet managed then find the management company

    NOTE - The mc-fleetman is never EVER set to its own mc-serial.

       Prior to MK02 system option marketing links the FM company drove 
       everything but with MK02 any contact, FM or client company, may be
       the link bod and we always use what we have defined for the client
       company so we need to ascertain which mc serial we are going to 
       use and if we have none to work from ..... abort
------------------------------------------------------------------------*/
if mketcomp.mc-fleetman <> 0 then
do:
    find mketcomp#
        where mketcomp#.mc-serial = mketcomp.mc-fleetman no-lock no-error.
    if not available mketcomp# then
        return error "ERROR - Client FM Company not found in mickgcont.p".
    
    if capt-mk02-l = true then
        assign use-mcser-i = mketcomp.mc-serial.
    else
        assign use-mcser-i = mketcomp#.mc-serial.
end.
else
    assign use-mcser-i = mketcomp.mc-serial.

if use-mcser-i = 0 then
    return error "ERROR - Marketing company use-mcser-i = 0 in mickgcont.p".

assign use-driver-l   = false
       copy-to-fm-l   = false
       branch-l       = false
       fm-cc-c        = ""
       fm-mco-id-i    = 0
       escalate-l     = false
       fleetmanaged-l = (available mketcomp#)
       where-c        = ""
       branch-code-c  = ""
       branch-desc-c  = ""
       branch-email-c = ""
       edi-funct-c    = ""
       letter-i       = 0
       let-pdf-i      = 0
       letter-c       = ""
       ip-addr-c      = ""
       op-addr-c      = ""
       postcode-c     = ""
       tmp-addr-c     = ""
       pdffile-c      = ""
       loop-i         = 0
       head-i         = 0
       optio-l        = false
       sms-l          = false
       mn-ser-i       = 0
       mob-num-c      = ""
       mketnadd-r     = ?
       comp-addr-l    = false
       contact-tt-r   = ?
       outmeth-i      = 6     /* default output to Printed */
       mketlink-r     = ?
       mketcont-r     = ?
       .
/*------------------------------------------------------
what method are we using for the company for this output
-------------------------------------------------------*/
find mketcomd
    where mketcomd.mc-serial = datain-tt.mcserial-i
    and   mketcomd.mch-proc  = datain-tt.mch-proc-i
    no-lock no-error.
if available mketcomd then
    assign outmeth-i = mketcomd.mcm-code.

/*---------------------------------------------------------------------
DRIVER CORRESPONDANCE - find out if its going to the driver and for
                        enhanced 'links' (MK02) we may need a copy 
                        of the letter to go to the fleet manager

            RECALL (39), SERVICE (40), MOT (41), & VED (65)

    NOTE - Driver flags are on the client company ONLY for both old
           and new MK02 links so use the input serial
----------------------------------------------------------------------*/
if lookup(string(datain-tt.mch-proc-i), "39,40,41,65") > 0 then
do:
    if capt-mk02-l then
        run mket/mick/mickcorr2.p (input  datain-tt.mcserial-i,
                                   input  datain-tt.let-type-c,
                                   input  datain-tt.mketdept-c,
                                   input  datain-tt.mketlocn-c,
                                   output use-driver-l,
                                   output copy-to-fm-l).
    else
        run mket/mick/mickcorr.p (input  datain-tt.mcserial-i,
                                  input  datain-tt.let-type-c,
                                  input  datain-tt.mketdept-c,
                                  input  datain-tt.mketlocn-c,
                                  output use-driver-l).
    /*------------------------------------------------------------
    Check for Car Rental contract - VED renewals and Recalls only
    ------------------------------------------------------------*/
    if lookup(string(datain-tt.mch-proc-i), "39,65") > 0 then
    do:
        run xftm/xvrc/xvrccrbranch.p (input  datain-tt.contract-r,
                                      output branch-code-c,
                                      output branch-desc-c,
                                      output branch-email-c).

        if branch-code-c <> "" then
        do:
            find hdbranch
                where hdbranch.hdb-code = branch-code-c
                no-lock no-error.
            if not available hdbranch then
                return error "ERROR - CR Branch not found in mickgcont.p".
            assign branch-l = true.
        end.
    end.
    /*----------------------------------------------------------------
    if we have a contract then set variables for the letter production
       as far as the rest goes we only need to know if its to driver
    ----------------------------------------------------------------*/
    if datain-tt.contract-r <> ? 
        and POL-lets-l then
    do:
        case datain-tt.mch-proc-i:
            when 39 then run LETTER-RCAL-ip in this-procedure.
            when 40 then run LETTER-SERV-ip in this-procedure.
            when 41 then run LETTER-MOT-ip  in this-procedure.
            when 65 then run LETTER-VED-ip  in this-procedure.
        end case.    

        if branch-l then
            run POL-BRANCH-ip in this-procedure.
        else
            run POL-CUST-ip in this-procedure.
    end.
end.   /* end of driver correspondence */

/*--------------------------------
Now for non letter production bits
---------------------------------*/
if contact-tt-r = ? then
do:
    find mketlink 
        where mketlink.mc-serial = use-mcser-i
        and   mketlink.ml-link   = datain-tt.ml-link-i
        no-lock no-error.
    if available mketlink then
        find mketcont 
            where mketcont.mco-id = mketlink.mco-id
            no-lock no-error.
    /*--------------------
    Credit control letters
    ---------------------*/
    if datain-tt.mch-proc-i = 43 then
    do:
        if  available slaccdet then
        do:
            {sysm/misc/misclnos.i
                 &create    = true
                 &en-code   = "''"
                 &lnos-code = "ZZZZ"
                 &max-1     = "999"
                 &next-1    = let-pdf-i
            }
            assign pdffile-c = mketcomh.mch-prefix
                             + slaccdet.sl-account
                             + string(let-pdf-i)
                             + string(use-mcser-i)
                             + "."
                             + mketcomh.mch-ext
                             .
        end.
    end.
    create contact-tt.
    assign contact-tt.key-c       = datain-tt.tt-key-c
           /*---------------------------------------
           New additional fields to tt
           NOTE - ANY cc list goes in the bcc field
           ---------------------------------------*/
           contact-tt.mcserial-i  = use-mcser-i
           contact-tt.bcc-list-c  = contact-tt.bcc-list-c
                                  + (if contact-tt.bcc-list-c <> "" then
                                        ";" else "")
                                  + fm-cc-c
           contact-tt.mch-proc-i  = datain-tt.mch-proc-i
           contact-tt.proc-desc-c = mketcomh.mch-desc
           contact-tt.ml-link-i   = datain-tt.ml-link-i
           contact-tt.link-desc-c = (if available statcode then
                                         statcode.stat-24-desc
                                     else "")
           contact-tt.mcm-code-i  = 0
           contact-tt.method-c    = ""
           contact-tt-r           = rowid(contact-tt)
           contact-tt.pdf-file-c  = pdffile-c
           contact-tt.mketcont-r  = (if available mketcont then
                                        rowid(mketcont)
                                     else ?)
           contact-tt.mketcont-re = (if available mketcont then
                                        recid(mketcont)
                                     else ?)
           .
end.
if available mketcomd then
do:
    find mketcomm
        where mketcomm.mcm-code =  mketcomd.mcm-code
        no-lock no-error.
    if available mketcomm then
        /*---------------------------------------------------
        Only set output if not already evaluated from letters
        ---------------------------------------------------*/
        assign contact-tt.output-i   = outmeth-i
               contact-tt.mcm-code-i = mketcomm.mcm-code
               contact-tt.method-c   = mketcomm.mcm-desc
               .
end.
else 
    assign contact-tt.output-i   = 6
           contact-tt.mcm-code-i = 6
           contact-tt.method-c   = "Printed"
           .
/*--------------------------------------------------------------------------
Now deal with other marketing links processing requirements

    NOTE - The company required may be the FM company for the old processing
           method but not for MK02 where the link person may be for either
           the client or fleet management company so use variable 

           SL DD notifications (32) require special processing for ACVM
---------------------------------------------------------------------------*/
if datain-tt.mch-proc-i = 32 then
do:
    if available sldebit2
        and capt-mk02-l then
    do:
        find first ddnot-mketbacc
            where ddnot-mketbacc.mc-serial    = use-mcser-i
            and   ddnot-mketbacc.mb-serial   <> ?
            and   ddnot-mketbacc.mac-bank-acc = sldebit2.sd2-bank-acc
            and   ddnot-mketbacc.mk-sort-code = sldebit2.sd2-sortcode
            no-lock no-error.

        if avail ddnot-mketbacc then
        do:
            find first ddnot-mketdept
                where ddnot-mketdept.mc-serial  = ddnot-mketbacc.mc-serial
                and   ddnot-mketdept.mcd-mb-ser = ddnot-mketbacc.mb-serial
                no-lock no-error.
            if available ddnot-mketdept then
               find mketlink
                   where mketlink.mc-serial = ddnot-mketbacc.mc-serial
                   and   mketlink.ml-link   = datain-tt.ml-link-i
                   and   mketlink.ml-locn   = ""
                   and   mketlink.ml-dept   = ddnot-mketdept.mcd-dept
                   and   mketlink.ml-seq    = 1
                   no-lock no-error.
        end.
    end.
    if not available mketlink then
    do:
        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input datain-tt.ml-link-i,
                                              input datain-tt.mketlocn-c,
                                              input datain-tt.mketlocn-c,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        if mketlink-r <> ? then
            find mketlink 
                where rowid(mketlink) = mketlink-r 
                no-lock no-error.
        
    end.
end.
else
do:
    assign  mketlink-r = ?
            mketcont-r = ?
            .
    run get-contact-ip in this-procedure (input use-mcser-i,
                                          input datain-tt.ml-link-i,
                                          input datain-tt.mketlocn-c,
                                          input datain-tt.mketlocn-c,
                                          input 1,
                                          output mketlink-r,
                                          output mketcont-r).
    if mketlink-r <> ? then
        find mketlink 
            where rowid(mketlink) = mketlink-r 
            no-lock no-error.
end.
/*--------------------------------------------------------
SL Statements (49) - Missing link default back to Invoices
---------------------------------------------------------*/
if datain-tt.mch-proc-i = 49  
    and not available mketlink then
do:
    assign datain-tt.ml-link-i = 30
           mketlink-r          = ?
           mketcont-r          = ?
           .
    run get-contact-ip in this-procedure (input use-mcser-i,
                                          input datain-tt.ml-link-i,
                                          input datain-tt.mketlocn-c,
                                          input datain-tt.mketlocn-c,
                                          input 1,
                                          output mketlink-r,
                                          output mketcont-r).
    if mketlink-r <> ? then
    do:
        find mketlink 
            where rowid(mketlink) = mketlink-r 
            no-lock no-error.
        find mketcont
            where mketcont.mco-id = mketlink.mco-id
            no-lock no-error.
        if available mketcont then
            assign contact-tt.mketcont-r  = rowid(mketcont)
                   contact-tt.mketcont-re = recid(mketcont)
                   .
    end.
end.

/*-----------------------------------------------------------------
No specific link for the following then revert to the fleet manager

       Contracts - Initial invs           - 3
                   Schedules/endorsements - 34
                   DD mandate             - 35
                   Pro forma invoices     - 44
       Ordering    Customer order conf    - 36
                   Customer request conf  - 60
       Service reminders                  - 40
       MOT letters                        - 41
       VED renewal letters                - 65
       Quotations                         - 69

    for Credit control letters (43) revert to the Invoice person
------------------------------------------------------------------*/
if not available mketlink
   and lookup(string(datain-tt.mch-proc-i), 
                         "3,34,35,44,36,60,40,41,65,69,43") > 0 then
do:
    assign datain-tt.ml-link-i = (if datain-tt.mch-proc-i = 43 then 30
                                  else 22)
           mketlink-r          = ?
           mketcont-r          = ?
           .
    run get-contact-ip in this-procedure (input use-mcser-i,
                                          input datain-tt.ml-link-i,
                                          input datain-tt.mketlocn-c,
                                          input datain-tt.mketlocn-c,
                                          input 1,
                                          output mketlink-r,
                                          output mketcont-r).
    if mketlink-r <> ? then
    do:
        find mketlink 
            where rowid(mketlink) = mketlink-r 
            no-lock no-error.
        find mketcont
            where mketcont.mco-id = mketlink.mco-id
            no-lock no-error.
        if available mketcont then
            assign contact-tt.mketcont-r  = rowid(mketcont)
                   contact-tt.mketcont-re = recid(mketcont)
                   .
    end.
end.
/*-----------------------------------------------
Do NOT override info already set from POL letters
------------------------------------------------*/
if available mketlink
    and contact-tt.fullname-c = "" then
do:
    /*---------------------------------------
    set the contact email address if possible
    ----------------------------------------*/
    find mketcont
        where mketcont.mco-id = mketlink.mco-id no-lock no-error.
    if available mketcont
        and mketcont.mco-historic  = false
        and mketcont.mco-edi-name <> ""
        and mketcont.mco-edi      <> 0 then
    do:
        assign contact-tt.name-c      = mketcont.mco-name
               contact-tt.inits-c     = mketcont.mco-inits
               contact-tt.title-c     = mketcont.mco-title
               contact-tt.fullname-c  = mketcont.mco-fullname
               contact-tt.compname-c  = mketcomp.mc-shortname
               contact-tt.mketcont-r  = rowid(mketcont)
               contact-tt.mketcont-re = recid(mketcont) 
               .
        find mkettele
            where mkettele.mc-serial = mketcont.mc-serial
            and   mkettele.mt-serial = mketcont.mco-edi
            no-lock no-error.
        if available mkettele then
            assign contact-tt.email-c = mketcont.mco-edi-name
                                      + "@"
                                      +  mkettele.mt-phone
                                      .
    end.
    /*------------------------------------------------------------------
    if we have no contact email address and no generic then set to print
    ------------------------------------------------------------------*/
    else if mketlink.ml-def-email = "" then
        assign outmeth-i = 6.
    /*----------------------------------------------------------------
    Any 'generic' override email address overrides that of the contact
    ----------------------------------------------------------------*/
    assign contact-tt.email-c    = (if mketlink.ml-def-email <> ""
                                        then mketlink.ml-def-email
                                    else contact-tt.email-c)
           contact-tt.ml-link-i  = mketlink.ml-link
           contact-tt.bcc-list-c  = contact-tt.bcc-list-c
                                 + (if contact-tt.bcc-list-c <> "" then ";"
                                    else "")
                                 + mketlink.ml-cc-list
           contact-tt.mcm-code-i =  outmeth-i
           .
end.
/*----------------------------------------------------
set the driver indicators if set on the client company 
-----------------------------------------------------*/
if (mketcomp.mc-vren-k = true
        and contact-tt.mch-proc-i = 42)
    or (mketcomp.mc-ved-k = true
            and contact-tt.mch-proc-i = 65)
    or (mketcomp.mc-mot1-k = true
            and contact-tt.mch-proc-i = 41)
    or (mketcomp.mc-rcal1-k = true
            and contact-tt.mch-proc-i = 39)
    or (mketcomp.mc-serv-k = true
            and contact-tt.mch-proc-i = 40) then
    assign contact-tt.method-c = "** Driver **".
/*---------------------------------------------------------
if we have no address then get it based on known parameters
----------------------------------------------------------*/
if contact-tt.address-c[1]
   + contact-tt.address-c[2] = "" then
do:
    /*------------------------------------------------------------------
    NOTE - If no SL account passed in there can be no 'C/O' modification
    -------------------------------------------------------------------*/
    run get-address-ip in this-procedure(input  datain-tt.sl-acc-c,
                                         input  contact-tt.mcserial-i,
                                         input  contact-tt.ml-link-i,
                                         input  datain-tt.mketdept-c,
                                         input  datain-tt.mketlocn-c,
                                         input  ?,
                                         output contact-tt.postcode-c
                                        ) no-error.
    if error-status:error then message return-value view-as alert-box.

    assign contact-tt.mketnadd-re  = mickaddr-nadd-r
           contact-tt.address-c[1] = a-addr[1]
           contact-tt.address-c[2] = a-addr[2]
           contact-tt.address-c[3] = a-addr[3]
           contact-tt.address-c[4] = a-addr[4]
           contact-tt.address-c[5] = a-addr[5]
           contact-tt.address-c[6] = a-addr[6]
           contact-tt.address-c[7] = a-addr[7]
           .
end.
/*---------------------------------------------
set these at the end - mainly for 'link' report
----------------------------------------------*/
assign contact-tt.dept-c = datain-tt.mketdept-c
       contact-tt.locn-c = datain-tt.mketlocn-c
       .
return.

/*---------------------------------------------------------------------
                     INTERNALS
----------------------------------------------------------------------*/
procedure LETTER-MOT-ip:

    if datain-tt.let-type-c = "MOT4" then
        assign escalate-l = true.

    {sysm/misc/misclnos.i
        &create    = true
        &en-code   = "''"
        &lnos-code = "ZZZZ"
        &max-1     = "999"
        &next-1    = let-pdf-i
    }
    assign datain-tt.ml-link-i = (if escalate-l then 23 else 44)
           letter-i            = 41
           letter-c            = contract.sl-account + string(let-pdf-i)
           where-c             = (if escalate-l then "Event escalator"
                                  else "MOT administrator")
           edi-funct-c         = "MOTs"
           .
    return.
end procedure.

procedure LETTER-VED-ip:

    assign datain-tt.ml-link-i  = 43
           letter-i             = 65 
           letter-c             = ""
           datain-tt.let-type-c = "VED"
           where-c              = "VED Contact"
           edi-funct-c          = "VEds"
           .
    return.
end procedure.

procedure LETTER-RCAL-ip:
    /*--------------------------------------------------------
    letter 1 & 2 to driver but 3 always goes to escalation bod
       therefore NO copy email to fleet manager is required
    ---------------------------------------------------------*/
    if datain-tt.let-type-c = "RCAL3" then
        assign escalate-l   = true
               copy-to-fm-l = false.

    assign datain-tt.ml-link-i = (if escalate-l then 23 else 22)
           letter-i            = 39
           edi-funct-c         = "Recalls"
           .
    return.
end procedure.

procedure LETTER-SERV-ip:

    if datain-tt.let-type-c = "?" then
        assign escalate-l = true.

    assign datain-tt.ml-link-i = (if escalate-l then 23 else 42)
           letter-i            = 40
           edi-funct-c         = "Serv"
           .
    return.
end procedure.

procedure POL-BRANCH-ip:

    define buffer hdbranch# for hdbranch.
    define buffer ip-mketcomp# for mketcomp.
    define buffer ip-mketnadd# for mketnadd.


    find hdbranch#
        where hdbranch#.hdb-code = branch-code-c
        no-lock no-error.
    if not available hdbranch# then return.

    /*----------------------------------------------------------------
    If its going to the Branch then we need to pack the Branch address
    ----------------------------------------------------------------*/
    assign ip-addr-c[1] = hdbranch#.hdb-address[1]
           ip-addr-c[2] = hdbranch#.hdb-address[2]
           ip-addr-c[3] = hdbranch#.hdb-address[3]
           ip-addr-c[4] = hdbranch#.hdb-address[4]
           ip-addr-c[5] = hdbranch#.hdb-county
           ip-addr-c[6] = hdbranch#.hdb-postcode
           a-addr       = ""
           .
    {sysm/incl/incladdr.i
        &addr     = ip-addr-c
        &county   = ip-addr-c[5]
        &postcode = ip-addr-c[6]
        &work-var = tmp-addr-c
        &head     = head-i
        &loop     = loop-i
        &out-addr = a-addr
    }

    find ip-mketcomp# 
        where ip-mketcomp#.mc-serial = use-mcser-i no-lock no-error.
    if available ip-mketcomp# then
        find ip-mketnadd#
            where ip-mketnadd#.mn-serial = ip-mketcomp#.mn-serial
            no-lock no-error.

    create contact-tt.
    assign contact-tt.key-c        = datain-tt.tt-key-c
           contact-tt.name-c       = hdbranch#.hdb-desc
           contact-tt.inits-c      = ""
           contact-tt.title-c      = ""
           contact-tt.fullname-c   = hdbranch#.hdb-desc
           contact-tt.addresse-c   = "Branch supervisor"
           contact-tt.mobile-c     = ""
           contact-tt.email-c      = hdbranch#.hdb-email
           contact-tt.address-c[1] = a-addr[1]
           contact-tt.address-c[2] = a-addr[2]
           contact-tt.address-c[3] = a-addr[3]
           contact-tt.address-c[4] = a-addr[4]
           contact-tt.address-c[5] = a-addr[5]
           contact-tt.postcode-c   = hdbranch#.hdb-postcode
           /*-------------------------------
              39 =  RCAL  and  65 = VED   
           --------------------------------*/
           contact-tt.output-i     = (if datain-tt.mch-proc-i = 65 then 6
                                        else
                                        (if datain-tt.mch-proc-i = 39 
                                           and datain-tt.let-type-c = "RCAL1"
                                                then 6
                                         else (if hdbranch.hdb-email <> ""
                                                    then 1
                                               else 6)
                                        )
                                     )
           contact-tt.pdf-file-c   = ""
           contact-tt.where-c      = "CR branch"
           contact-tt.compname-c   = (if available ip-mketnadd# then
                                          ip-mketnadd#.mn-name
                                      else
                                          if available ip-mketcomp# then
                                               ip-mketcomp#.mc-shortname
                                      else "")
           contact-tt.mketcomp-r   = ?
           contact-tt.mketcomp-re  = ?
           contact-tt.mketcont-r   = ?
           contact-tt.mketcont-re  = ?
           contact-tt.mketnadd-r   = ?
           contact-tt.mketnadd-re  = ?
           contact-tt.hdbranch-r   = rowid(hdbranch#)
           /*---------------------------------------------
           New additional fields
           NOTE - No copy to fleet manager or anyone else
           ----------------------------------------------*/
           contact-tt.mcserial-i   = use-mcser-i
           contact-tt.bcc-list-c   = ""
           contact-tt.mch-proc     = datain-tt.mch-proc-i
           contact-tt.proc-desc-c  = mketcomh.mch-desc
           contact-tt.ml-link-i    = datain-tt.ml-link-i
           contact-tt.link-desc-c  = ""
           contact-tt.mcm-code-i   = contact-tt.output-i
           contact-tt.method-c     = "Branch"
           contact-tt-r            = rowid(contact-tt)
           outmeth-i               = contact-tt.output-i
           optio-l                 = false
           pdffile-c               = "".

    return.
end procedure.   /*  POL-BRANCH-ip */

procedure POL-CUST-ip:

    define buffer ip-mketcont# for mketcont.
    define buffer fm-mketcont# for mketcont.
    define buffer ip-mketlink# for mketlink.
    define buffer fm-mketlink# for mketlink.
    define buffer ip-mketcomp# for mketcomp.
    define buffer ip-mketnadd# for mketnadd.
    define buffer ip-mkettele# for mkettele.
    define buffer ip-drivlink# for drivlink.

    define variable mketlink-r as rowid     no-undo.
    define variable mketcont-r as rowid     no-undo.
    define variable ipemail-c  as character no-undo.
    define variable ipcclist-c as character no-undo.
    define variable emailerr-c as character no-undo.

    find ip-mketcomp#
        where ip-mketcomp#.mc-serial = use-mcser-i no-lock no-error.
    /*----------------------------------------------------------------
    If the 'driver direct' flag is set for the company then the copy 
    email to the fleet manager may be set - find the relevant contact

    NOTE - The link mco-id may now be for either the client company
           or the fleet management company..so find whoever it is
           At this stage any 'email cc list' is being ignored we are
           doing a copy to the fleet manager NOT the fleet manager 
           and all his dogs!
    -----------------------------------------------------------------*/
    if copy-to-fm-l then
    do:
        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input 22,
                                              input contract.c-dept,
                                              input contract.c-locn,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        find fm-mketlink#
            where rowid(fm-mketlink#) = mketlink-r
            no-lock no-error.
        find fm-mketcont#
            where rowid(fm-mketcont#) = mketcont-r
            no-lock no-error.
        /*-------------------------------------------------------------
        record fleet manager contact id - mainly for RCAL 'copy' emails
        as recalls are always printed with a dealer completion sheet

        NOTE - There may NOT be a contact if the mketlink has a
               'generic' override email address
        --------------------------------------------------------------*/
        if available fm-mketcont# then
            assign fm-mco-id-i = fm-mketcont#.mco-id
                   .
        /*----------------------------------------------
        'generic' override overrides ANY contact details
        -----------------------------------------------*/
        if available fm-mketlink#
            and fm-mketlink#.ml-def-email <> "" then
        do:
            assign fm-cc-c = fm-mketlink#.ml-def-email.
            run sysm/misc/miscemlv.p(input-output fm-cc-c) no-error.
            if error-status:error
                or return-value <> "" then 
                assign fm-cc-c = "".
        end.

        if fm-cc-c = ""
            and available fm-mketcont# 
            and fm-mketcont#.mco-edi-name <> "" then
        do:
            find first ip-mkettele#
                where ip-mkettele#.mc-serial = fm-mketcont#.mc-serial
                and   ip-mkettele#.mt-serial = fm-mketcont#.mco-edi
                no-lock no-error.
            if avail ip-mkettele# then
            COPY_FM:
            do:
                assign ipemail-c =  fm-mketcont#.mco-edi-name
                                 + "@"
                                 + ip-mkettele#.mt-phone
                                 .
                run sysm/misc/miscemlv.p(input-output ipemail-c) no-error.
                if error-status:error
                    or return-value <> "" then leave COPY_FM.

                assign fm-cc-c = fm-cc-c   
                                 + (if fm-cc-c <> "" then ";" else "")
                                 + ipemail-c
                                 .
            end.
        end.           
    end.    /* copy-to-fm logical */

    if fleetmanaged-l 
        and datain-tt.override-c = ""
        and use-driver-l = false then
    FLEET-BLK:
    do:
        /*--------------------------------------
        find the contact for the specified link
        --------------------------------------*/
        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input datain-tt.ml-link-i,
                                              input contract.c-dept,
                                              input contract.c-locn,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        find ip-mketlink#
            where rowid(ip-mketlink#) = mketlink-r
            no-lock no-error.
        find ip-mketcont#
            where rowid(ip-mketcont#) = mketcont-r
            no-lock no-error.
        
        if available ip-mketcont# then leave FLEET-BLK.

        /*-------------------------------------------
        If no specific contact then find the backstop
        -------------------------------------------*/
        assign where-c             = "Fleet administrator"
               datain-tt.ml-link-i = 22
               .
        /*--------------------------------------
        find the contact for the specified link
        --------------------------------------*/
        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input datain-tt.ml-link-i,
                                              input contract.c-dept,
                                              input contract.c-locn,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        find ip-mketlink#
            where rowid(ip-mketlink#) = mketlink-r
            no-lock no-error.
        find ip-mketcont#
            where rowid(ip-mketcont#) = mketcont-r
            no-lock no-error.
    end.
    else
    COMP-BLK:
    do:
        /*---------------------------------------------------------------
        NOTE - if SERVICE (40) escalation then no point in finding driver
        ---------------------------------------------------------------*/
        if datain-tt.mch-proc-i <> 40
            and escalate-l = false then
        do:
            /*------------------------------------
            get the latest driver for the contract
            -------------------------------------*/
            {cont/dlin/dlinfind.i &NoFind = @
                                  &Drivlink = ip-drivlink#}

            if (use-driver-l
                or datain-tt.override-c = "Driver")
                and available ip-drivlink# then
                find ip-mketcont#
                    where ip-mketcont#.mco-id = ip-drivlink#.mco-id
                    no-lock no-error.

            if available ip-mketcont# then
            do:
                assign where-c = "Driver".
                /*-------------------------------------------------------
                 The default is driver (use-driver-l variable) but we
                 want to override it to send to the company address, or
                 the default is company but its being overriden to driver
                ---------------------------------------------------------*/
                if (use-driver-l = true
                        and datain-tt.override-c <> "Customer")
                    or datain-tt.override-c = "Driver" then
                do:
                    run set-address-ip(input  datain-tt.let-type-c,
                                       input  use-mcser-i,
                                       input  ip-mketcont#.mco-id,
                                       output mn-ser-i).
                    find ip-mketnadd#
                        where ip-mketnadd#.mn-serial = mn-ser-i
                        no-lock no-error.
                    leave COMP-BLK.
                end.
            end.
        end.
        /*----------------------------------------
        if no driver contact go through the links
        ----------------------------------------*/
        assign where-c = (if datain-tt.ml-link-i = 44 then 
                              "MOT contact"
                          else (if datain-tt.ml-link-i = 43 then
                                    "VED contact"
                                else (if datain-tt.ml-link-i = 23 then
                                         "Event Escalator"
                                else "Fleet administator"))).

        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input datain-tt.ml-link-i,
                                              input contract.c-dept,
                                              input contract.c-locn,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        /*------------------------------------------
        find the link & again any generic set use it
        -------------------------------------------*/
        find ip-mketlink#
            where rowid(ip-mketlink#) = mketlink-r
            no-lock no-error.
        if available ip-mketlink# then
        do:
            assign ipcclist-c = ip-mketlink#.ml-cc-list.

            if ip-mketlink#.ml-def-email <> "" then
            do:
                assign ipemail-c  = ip-mketlink#.ml-def-email.
                leave COMP-BLK.
            end.
        end.

        find ip-mketcont#
            where rowid(ip-mketcont#) = mketcont-r
            no-lock no-error.
        /*------------------------------------------
        available contact and it's the fleet manager
        ------------------------------------------*/
        if available ip-mketcont#
            or datain-tt.ml-link-i = 22 then leave COMP-BLK.

        /*-----------------------------------
        final backstop is the 'Fleet manager'
        ------------------------------------*/
        assign where-c             = "Fleet administrator"
               datain-tt.ml-link-i = 22.

        run get-contact-ip in this-procedure (input use-mcser-i,
                                              input datain-tt.ml-link-i,
                                              input contract.c-dept,
                                              input contract.c-locn,
                                              input 1,
                                              output mketlink-r,
                                              output mketcont-r).
        find ip-mketlink#
            where rowid(ip-mketlink#) = mketlink-r
            no-lock no-error.
        if available ip-mketlink# then
            assign ipcclist-c = ip-mketlink#.ml-cc-list.

        find ip-mketcont#
            where rowid(ip-mketcont#) = mketcont-r
            no-lock no-error.
    end.

    create contact-tt.
    assign contact-tt.key-c       = datain-tt.tt-key-c
           contact-tt-r           = rowid(contact-tt)
           /*-------------------------
           New additional fields to tt
           -------------------------*/
           contact-tt.mcserial-i  = use-mcser-i
           contact-tt.bcc-list-c  = ""
           contact-tt.mch-proc-i  = datain-tt.mch-proc-i
           contact-tt.ml-link-i   = datain-tt.ml-link-i
           contact-tt.link-desc-c = ""
           contact-tt.mcm-code-i  = 0
           contact-tt.method-c    = ""
           .

    if available ip-mketcont# then
    do:
        /*-----------------------------------------------------
        if its a driver check for & if we can send sms message
        ------------------------------------------------------*/
        if (use-driver-l = true
               and datain-tt.override-c <> "Customer")
            or datain-tt.override-c = "Driver" then
        do:
            run lets/csms/csmschek.p (input  use-mcser-i,
                                      input  ip-mketcont#.mco-id,
                                      output sms-l).
            if sms-l then
            do:
                assign outmeth-i = 2.
                if datain-tt.let-type-c = "MOT" then
                    run p_smsTemplateCheck(input        datain-tt.let-type-c,
                                           input-output sms-l).

                run p_processPrefs(input        datain-tt.let-type-c,
                                   input-output sms-l,
                                   input-output outmeth-i,
                                   output       mob-num-c).
            end.
        end.

        /*----------------------------------------
         only send by SMS if being sent to driver
        -----------------------------------------*/
        if outmeth-i <> 2 then
        do:
            /*--------------------------------------------------
            if service direct escalation change output to email 
            except that 'service direct' is ALL manual!
            Nothing is dealt with automatically by the system.

            NOTE - Gets work email address in pref to home 
            --------------------------------------------------*/
            if datain-tt.let-type-c = "Service"
                and escalate-l = true then
            do:
                assign outmeth-i = 1.
                if ip-mketcont#.mco-edi <> 0
                    and ip-mketcont#.mco-edi-name <> "" then
                do:
                    {mket/mtel/mtelemal.i
                        &mcser = ip-mketcont#.mc-serial
                        &mtser = ip-mketcont#.mco-edi
                        &email = ipemail-c
                    }
                    if ipemail-c <> "" then
                        assign ipemail-c  = ip-mketcont#.mco-edi-name
                                        + "@"
                                        + ipemail-c.

                    else
                        assign ipemail-c  = ip-mketcont#.mco-email.
                end.
            end.
            else
            do:
                /*------------------------
                get method of printing etc 
                -------------------------*/
                run lets/lick/lickmth2.p (input  contract.sl-account,
                                          input  recid(ip-mketcont#),
                                          input  letter-i,
                                          input  letter-c,
                                          input  use-driver-l,
                                          output outmeth-i,
                                          output optio-l,
                                          output pdffile-c,
                                          output ipemail-c).
            end.
        end.
        /*--------------------------------------------------------------
         if email but no email address (work or home) change to printed
             or when called by vehicle recalls change to printed
              so manufacturer notice can be sent with the letter
         -------------------------------------------------------------*/
        if (outmeth-i = 1
                and ip-mketcont#.mco-edi-name = ""
                and ip-mketcont#.mco-email    = "")
            or (outmeth-i = 2 and ip-mketcont#.mco-sms-mk = false)
            or datain-tt.let-type-c = "RCAL1" then
            assign outmeth-i = 6.
    end.

    /*---------------------------------------------------
    mn-ser-i will ONLY be set if its going to the driver
    ----------------------------------------------------*/
    find ip-mketnadd#
        where ip-mketnadd#.mn-serial = mn-ser-i no-lock no-error.

    /*----------------------------------------------------------------
     will only be available if being sent to the driver and the driver
     address is populated, if its not send it to the company address
    ----------------------------------------------------------------*/
    if not available ip-mketnadd#
        or  (available ip-mketnadd#
             and ip-mketnadd#.mn-address[1] = ""
             and ip-mketnadd#.mn-address[2] = "") then
    do:
        release ip-mketnadd#.
        assign a-addr = "".
        /*--------------------------------------------------------
        find and modify the address as reqd for FM managed company
        packed address is in a-addr array 1 to 7
        ---------------------------------------------------------*/
        run get-address-ip in this-procedure(input  datain-tt.sl-acc-c,
                                             input  contact-tt.mcserial-i,
                                             input  contact-tt.ml-link-i,
                                             input  datain-tt.mketdept-c,
                                             input  datain-tt.mketlocn-c,
                                             input  ?,
                                             output contact-tt.postcode-c
                                            ).

        assign contact-tt.mketnadd-re  = mickaddr-nadd-r
               contact-tt.address-c[1] = a-addr[1]
               contact-tt.address-c[2] = a-addr[2]
               contact-tt.address-c[3] = a-addr[3]
               contact-tt.address-c[4] = a-addr[4]
               contact-tt.address-c[5] = a-addr[5]
               contact-tt.address-c[6] = a-addr[6]
               contact-tt.address-c[7] = a-addr[7]
               .
    end.

    /*---------------------------------------
     if sending an email make sure its valid
    ----------------------------------------*/
    if ipemail-c <> "" then
    do:
        run sysm/misc/miscemlv.p (input-output ipemail-c) no-error.
        /*-----------------------------------------------
        if the return-value contains anything the email
        address is invalid - change the output to printed
        ------------------------------------------------*/
        if error-status:error
            or return-value <> "" then
            assign ipemail-c = ""
                   outmeth-i  = 6.
    end.

    assign contact-tt.name-c       = (if avail(ip-mketcont#) then
                                          ip-mketcont#.mco-name
                                      else "")
           contact-tt.inits-c      = (if avail(ip-mketcont#) then
                                          ip-mketcont#.mco-inits
                                      else "")
           contact-tt.title-c      = (if available ip-mketcont# then
                                           ip-mketcont#.mco-title
                                      else "")
           contact-tt.fullname-c   = (if available ip-mketcont#
                                          and sms-l = true then mob-num-c
                                      else
                                         (if available ip-mketcont# then
                                              ip-mketcont#.mco-fullname
                                          else "Fleet Manager"))
           contact-tt.addresse-c   = (if available ip-mketcont# then
                                          ip-mketcont#.mco-dear
                                      else "Fleet Manager")
           contact-tt.mobile-c     = (if available ip-mketcont# then
                                          mob-num-c
                                      else "")
           contact-tt.addresse-c   = (if (avail(ip-mketcont#)
                                          and contact-tt.addresse-c = "")
                                          then ip-mketcont#.mco-inits
                                      else contact-tt.addresse-c)
           contact-tt.email-c      = replace(ipemail-c,"@@","@")
           contact-tt.output-i     = (if outmeth-i <> 0 then outmeth-i
                                      else 6)
           contact-tt.pdf-file-c   = pdffile-c
           contact-tt.where-c      = where-c

           contact-tt.comp-addr-l  = (where-c = "Driver"
                                          and comp-addr-l = true)
           contact-tt.personal-l   = (available contract 
                           and lookup(contract.cont-type, "PCH,PCZ,PCC") > 0)

           contact-tt.compname-c   = (if contact-tt.personal-l = true
                                        or available ip-mketnadd# then
                                          ip-mketnadd#.mn-name
                                      else (if available ip-mketcomp# then
                                               ip-mketcomp#.mc-shortname
                                            else ""))
         
           contact-tt.mketcomp-r   = rowid(ip-mketcomp#)
           contact-tt.mketcomp-re  = recid(ip-mketcomp#)
           contact-tt.mketcont-r   = (if available ip-mketcont# then
                                          rowid(ip-mketcont#)
                                      else ?)
           contact-tt.mketcont-re  = (if available ip-mketcont# then
                                          recid(ip-mketcont#)
                                      else ?)
           contact-tt.mketnadd-r   = (if available ip-mketnadd# then
                                          rowid(ip-mketnadd#)
                                      else ?)
           contact-tt.mketnadd-re  = (if available ip-mketnadd# then
                                          recid(ip-mketnadd#)
                                      else ?)
           contact-tt.fleet-l      = fleetmanaged-l
           contact-tt.bcc-list-c   = contact-tt.bcc-list-c
                                   + (if contact-tt.bcc-list-c <> ""
                                          then ";" else "")
                                   + fm-cc-c
           contact-tt.bcc-list-c   = contact-tt.bcc-list-c
                                   + (if contact-tt.bcc-list-c <> ""
                                          then ";" else "")
                                   + ipcclist-c
           contact-tt.fm-mcoid-i   = fm-mco-id-i
           .

    return.
end procedure.    /* POL-CUST-ip */

procedure set-address-ip:

    define input  parameter ip-type      as character no-undo.
    define input  parameter use-mcser-i  as integer   no-undo.
    define input  parameter use-mcoid-i  as integer   no-undo.
    define output parameter op-mn-serial as integer   no-undo.

    define variable use-comp-l as logical no-undo.

    define buffer ip-mketcomp# for mketcomp.
    define buffer ip-mketcont# for mketcont.

    find ip-mketcomp# 
        where ip-mketcomp#.mc-serial = use-mcser-i
        no-lock no-error.

    find ip-mketcont#
        where ip-mketcont#.mco-id = use-mcoid-i
        no-lock no-error.
    

    if available ip-mketcont# then
    do:
        assign use-comp-l = (ip-mketcont#.mco-work-add = 0
                                and ip-mketcont#.mco-address = 0).
        if not use-comp-l then
        do:
            case datain-tt.mch-proc-i:
                when 41                /* MOT  */ 
                    or when 39 then    /* RCAL */ 
                do:
                    /*----------------------------------------------------
                    use driver address if avail otherwise use work-address
                    -----------------------------------------------------*/
                    if ip-mketcont#.mco-address <> 0 then
                        assign op-mn-serial = ip-mketcont#.mco-address
                               comp-addr-l  = false.
                    else
                        assign op-mn-serial = ip-mketcont#.mco-work-add
                               comp-addr-l  = false.
                end.
                when 65 then         /* VED */
                do:
                    case ip-mketcont#.mco-rem-ved:
                        when "" or when "home" then
                        do:
                            assign op-mn-serial =
                                          (if ip-mketcont#.mco-address = 0
                                              then ip-mketcont#.mco-work-add
                                           else ip-mketcont#.mco-address).
                        end.
                        when "work" then
                        do:
                            assign op-mn-serial = 
                                          (if ip-mketcont#.mco-work-add = 0
                                               then ip-mketcont#.mco-address
                                           else ip-mketcont#.mco-work-add).
                        end.
                    end case.
                end.          /* when 65  (VED)       */
            end case.         /* datain-tt.mch-proc-i */
        end.                  /* not use company      */
    end.
 
    if op-mn-serial = 0 then
        assign op-mn-serial = use-mcser-i
               /*-----------------------------------------------
               this var defined and used in main body of routine
               -----------------------------------------------*/
               comp-addr-l  = true. 

    return.
end procedure. /* set-address-ip */

procedure get-contact-ip:

    define input  parameter mc-serial-i  as integer   no-undo.
    define input  parameter ml-link-i    as integer   no-undo.
    define input  parameter department-c as character no-undo.
    define input  parameter location-c   as character no-undo.
    define input  parameter priority-i   as integer   no-undo.

    define output parameter mketlink-r   as rowid     no-undo.
    define output parameter mketcont-r   as rowid     no-undo.
    
    /*--------------------------------------
    to handle mickfcon 'recid's not rowid's
    ---------------------------------------*/
    define variable ip-mketlink-r as recid no-undo.
    define variable ip-mketcont-r as recid no-undo.
    
    define buffer ip-mketcont# for mketcont.
    define buffer ip-mketlink# for mketlink.

    run mket/mick/mickfcon.p (input  mc-serial-i,
                              input  ml-link-i,
                              input  department-c,
                              input  location-c,
                              input  priority-i,
                              output ip-mketlink-r,
                              output ip-mketcont-r).
    /*------------------------
    Now swap things to a rowid
    -------------------------*/
    if ip-mketlink-r <> ? then
    do:
        find ip-mketlink#
            where recid(ip-mketlink#) = ip-mketlink-r
            no-lock no-error.
        if available ip-mketlink# then
            assign mketlink-r = rowid(ip-mketlink#).
    end.
    else
    do:
        /*-----------------------------------------------
        if no contact then there may be a generic address
           try to find by dept/location then default 
        ------------------------------------------------*/
        find ip-mketlink#
            where ip-mketlink#.mc-serial = mc-serial-i
            and   ip-mketlink#.ml-link   = ml-link-i
            and   ip-mketlink#.ml-locn   = location-c
            and   ip-mketlink#.ml-dept   = department-c
            and   ip-mketlink#.ml-seq    = priority-i
            no-lock no-error.
        if avail ip-mketlink# then
            assign mketlink-r = rowid(ip-mketlink#).
        else
        do:
            find ip-mketlink#
                where ip-mketlink#.mc-serial = mc-serial-i
                and   ip-mketlink#.ml-link   = ml-link-i
                and   ip-mketlink#.ml-locn   = ""
                and   ip-mketlink#.ml-dept   = ""
                and   ip-mketlink#.ml-seq    = 1
                no-lock no-error.
            if avail ip-mketlink# then
                assign mketlink-r = rowid(ip-mketlink#).
        end.
    end.
    if ip-mketcont-r <> ? then
    do:
        find ip-mketcont#
            where recid(ip-mketcont#) = ip-mketcont-r
            no-lock no-error.
        if available ip-mketcont# then
            assign mketcont-r = rowid(ip-mketcont#).
    end.

    return.
end procedure. /* get-contact-ip */

/*--------------
 get-address-ip
--------------*/
{mket/mick/mickaddget.i}

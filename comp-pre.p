/* set prog to a single program name e.g. xsurtele.p or to a comma
   separated list e.g. xsurtele.p,xsurenq.p */

&scoped-define PROG xcapvalidation.p

def stream sIn1.
def stream sIn2.

def var i as int.
def var cFullName as character no-undo.
def var cprogName as character no-undo.
def var cProgPath as character no-undo.

function fGetPath returns character
  (input ip_progName as character) forward.

do i = 1 to num-entries("{&PROG}", ","):
  
  assign cProgName = entry(i, "{&PROG}", ",")
         cProgPath = fGetPath(cProgName).
         
  if cProgPath = ? then
  do:
    message "Couldn't find program" cProgName view-as alert-box error.
    next.
  end.
  assign cFullName = fGetPath(cProgName) + "/" + cProgName.

  compile value(cFullName)
  preprocess value ("/home/jwm/ppro/" + cProgName + "pro") ~ no-error.

  if compiler:error then
  do:
    message "Error in compilation of " cFullName skip
    "View Errors? "
    view-as alert-box error
    buttons yes-no
    update response as logical.
    
    if response then
      message "compilation error in" compiler:file-name "at line"
               compiler:error-row 
               skip "column" compiler:error-col
      view-as alert-box information.
    
  end.
  else if compiler:warning then
    message "Warnings in compilation of " cFullname view-as alert-box.
  else
    message "Compiled " + cFullname + " successfully!"
    view-as alert-box .
end.

function fGetPath returns character
  (input ip_progName as character):

  def var cSearchDir as character no-undo.
  def var cFindDir as character no-undo.

  assign cFindDir = substring(ip_progName, 1, 4).

  def var i as int.
  def var cImportStream as character no-undo.
  def var cImportStream2 as character no-undo.
  def var cImportStream3 as character no-undo.

  do i = 1 to num-entries("/u2/lease2,/v2/lease2", ","):
    assign cSearchDir = entry(i, "/u2/lease2,/v2/lease2", ",").
    
    input stream sIn1 from os-dir(cSearchDir).
    repeat:
      import stream sIn1 cImportstream.

      assign file-info:file-name = cSearchDir + "/" + cImportStream.
      if file-info:file-type matches "*D*" 
        and cImportStream <> "." and cImportStream <> ".." then
      do:
        assign cImportStream2 = file-info:full-pathname.
        if file-info:file-type matches "*D*" 
        then
        do:
          input stream sIn2 from os-dir(cImportStream2).
          repeat:
              import stream sIn2 cImportStream3.
              file-info:file-name = cImportStream3.
                
              if cImportStream3 = cfindDir then
              do:
                if search(cImportStream2 + "/" 
                          + cImportStream3 + "/"
                  + ip_progName) <> ? then
                  return cImportStream2 + "/" + cImportStream3.
                else
                  next.
              end.                  
          end.
          input stream sIn2 close.
        end.
      end. /* file-type = "D" */
    end. /* repeat */
    input stream sIn1 close.
  end. /* do i = 1..... */
end function.  

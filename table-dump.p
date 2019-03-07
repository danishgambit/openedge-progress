def var i as int.
for each _file
  where _file._tbl-type = "T"
  no-lock:
  /*
      if _file-name begins "mk" // marketing
      or _file-name begins "u"  // user info
      or _file-name begins "mod" // models & prcing applied
      or _file-name begins "z" // menus ?
      or _file-name begins "cl" // clit.....
      or _file-name begins "ct" // ctrm      
  */
    if _file-name begins "q" 
    and not _file-name begins "qwm"
      then
      do:
      
        i = i + 1.
        run prodict/dump_d.p(input _file-name,
          input "/u4/qa1/shared/tmp/jim-db-copy/tables/",
          input "iso8859-1").
      end.

  end.

  message i view-as alert-box.
empty_:
        .cstr ";"

escape_:
        ; .cstr ";"
        .cstr "13\\E70(` `)13\\E`> `0\\$!;"

backsp_:
        .cstr "\\$@0=0=(1\\$\\-8\\E` `8\\E);"

toggleBase_:
        .cstr "\\b@0=\\b!;"

printStack_:
        .cstr "`=> `\\P\\N\\N`> `;"        

list_:
        .cstr "\\N26(\\i@\\Z)`> `;"

        ; DB "\\N26("
        ; DB      "\\2@\\i@{+@",$22
        ; DB      "\\@\\^;=0=("
        ; DB          "`:`\\^A\\j@+\\E"
        ; DB          "100("
        ; DB              $22,"\\i@+\\@"
        ; DB              $22,"\\E\\^;=0=\\W"
        ; DB          ")\\N"
        ; DB      ")"
        ; DB ");"

; editt_:
;         .cstr "\\1@\\@\\9! \\9@\\^A< \\9@\\^Z> |0= (0\\1! 13\\E);"
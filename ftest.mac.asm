
.macro utilDefs
    DB ":Q ",$22,";"                    ; ( n -- n n ) a convenient way to access " 
    DB ":W \\h@! 2\\h\\+;"              ; ( n -- ) compiles a word to heap
    DB ":K \\D\\t@- 0$ ($1+^);"         ; ( x1...xn num -- hash )
.endm

.macro arrayDefs
    DB  ":H 0\\R\\R ( $%@ 1+^ $ 2+)';"      ; arr len -- hash           hash array

    DB  ":R \\f! ( $%@ \\f@\\G $ 2+) ' ;"   ; v0 arr len fun -- val     reduce array

    DB  ":M \\f! \\h@\\R\\R "               ; arr len fun -- arr' len'  map array
    DB      "(Q@"
    DB      "\\f@\\G W 2+"
    DB      ")" 
    DB  "' \\h@ % -};"

    DB  ":F \\f! \\h@ \\R\\R "
    DB      "(Q@Q " 
    DB      "\\f@\\G "
    DB      "\\(W)(') 2+ "
    DB      ")"
    DB  " '  \\h@ % -};"

    DB  ":Z `[ `(Q @.2+)`]` ' ;"            ; arr len --                print array
.endm

.macro tester, name1, test1, expect1
    DB "`.`\\D\\t!"
    DB " ",test1," "
    DB "K\\D\\t!"                           ; ( -- hash1 )
    DB " ",expect1," "
    DB "K=0=(\\N`fail: ",name1," ",test1," expected: "
    DB expect1,"`\\N\\N",0,")"
.endm


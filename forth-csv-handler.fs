\ forth csv handler by gustav melck, nov 2022
\ vim: ft=forth fdm=marker

\ todo move away from cstrings and use dstrings instead, where there's a
\      whole leading cell instead of a char for the len

s" inc/forth-csv.fs" required
s" inc/forth-list-tools.fs" required

true value type-during-header-slurp?  \ when using rest-of-rec>list, type the contents of each cell

private{  \ {{{

2048 constant bmax

: gthrow  ( ior addr u -- )
    2 pick  if  type ." ; forth-csv-handler error " dup . cr throw  else  2drop drop  then  ;

create buf bmax chars allot
0 value blen

0 value new-line-handler
0 value field-handler
0 value not-virtual-eof?
false value buf-loaded?
false value last-not-eof?
0 value csvrec#

: tail-recursive  ( -- )  postpone if  postpone r>  postpone drop  postpone then  ;  immediate
: tail-recurse  ( -- )  postpone true  postpone recurse  ;  immediate

: >cstring  ( addr u -- addr' )
    dup 1+ chars allocate s" >cstring error" gthrow >r
    dup r@ c!  r@ 1+ swap cmove  r>  ;
: -cstring  ( addr -- )  free s" -cstring error" gthrow  ;

: exec-new-line-handler  ( -- )  buf blen new-line-handler execute  ;
: exec-field-handler  ( u -- )  \ u is the field#
    buf blen rot field-handler execute  ;

: (process-csv)  ( l? -- )  tail-recursive
    buf-loaded? 0=  if  buf bmax read-csv-field to last-not-eof? to blen  then  false to buf-loaded?
    not-virtual-eof?  if  buf blen not-virtual-eof? execute last-not-eof? and to last-not-eof?  then
    last-csv-field# case
        0   of  csvrec# 1+ to csvrec#
                new-line-handler  if  exec-new-line-handler
                else  0 exec-field-handler  then
            endof
        dup exec-field-handler
    endcase
    last-not-eof?  if  tail-recurse  then  ;

0 value item-counter
0 value list-head

create quotestr char " c,  \ "
create commastr char , c,  \ ,

: (csvtype)  ( addr u -- )  \ surround field by quotes if the text contains a quote
    2dup quotestr 1 search -rot 2drop  if
        quotestr 1 type  type  quotestr 1 type
    else  2dup commastr 1 search -rot 2drop  if
        quotestr 1 type  type  quotestr 1 type
    else  type  then then  ." ,"  ;

: (rest-of-rec>list)  ( l? -- )  tail-recursive
    buf-loaded? 0=  if  buf bmax read-csv-field to last-not-eof? to blen  then  false to buf-loaded?
    last-csv-field# 0= item-counter and  if  true to buf-loaded?  else
        type-during-header-slurp?  if  buf blen (csvtype)  then
        buf blen >cstring list-head +list to list-head
        item-counter 1+ to item-counter
        last-not-eof?  if  tail-recurse  then
    then  ;

}private  \ }}}

: new-line-handler!  ( xt -- )  \ xt must have this effect: ( addr u -- ), where addr u is the content of the field
    to new-line-handler  ;

: field-handler!  ( xt -- )
    \ xt must have this effect: ( addr u u' -- ), where addr u is the content of the field and u' is the field#
    to field-handler  ; 

: virtual-eof-checker!  ( xt -- )
    \ xt must have this effect: ( addr u -- f? ), where addr u is the field contents and when f? is true it means "not-eof" 
    to not-virtual-eof?  ;

: rest-of-rec>list  ( -- addr )
    \ can be called from the new-line-handler, for example, when a trigger indicating a range start is encountered
    \ addr is the head of the new list; the list should be freed with: addr -rec-list
    0 to item-counter  0 to list-head  false (rest-of-rec>list)  list-head reverse-list  0 list-head free-list  ;

: -rec-list  ( addr -- )  ?dup  if  ['] -cstring swap free-list  then  ;

: csvtype  ( addr u -- )  (csvtype)  ;  \ surround in quotes if needed; add comma after field

: csv-rec#  ( -- u )  \ record number
    csvrec#  ;

: process-csv  ( fid -- )  \ fid is the csv file id
    with-csv-file-id  -1 to csvrec#  false (process-csv)  ;

privatize


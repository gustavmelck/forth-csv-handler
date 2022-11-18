\ forth csv handler by gustav melck, nov 2022
\ vim: ft=forth fdm=marker

\ todo move away from cstrings and use dstrings instead, where there's a
\      whole leading cell instead of a char for the len

s" lib/forth-csv.fs" required
s" lib/forth-list-tools.fs" required

private{

2048 constant bmax

: gthrow  ( ior addr u -- )
    2 pick  if  type ." ; forth-csv-handler error " dup . cr throw  else  2drop drop  then  ;

create buf bmax chars allot
0 value blen

0 value new-line-handler
0 value field-handler

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
    buf bmax read-csv-field >r to blen
    last-csv-field# case
        0   of  new-line-handler  if  exec-new-line-handler  else  0 exec-field-handler  then  endof
        dup exec-field-handler
    endcase
    r>  if  tail-recurse  then  ;

0 value item-counter
0 value list-head

: (rest-of-rec>list)  ( l? -- )  tail-recursive
    buf bmax read-csv-field >r to blen
    last-csv-field# 0= item-counter and  if  r> drop  else
        buf blen >cstring list-head +list to list-head
        item-counter 1+ to item-counter
        r>  if  tail-recurse  then
    then  ;

}private

: new-line-handler!  ( xt -- )  \ xt must have this effect: ( addr u -- ), where addr u is the content of the field
    to new-line-handler  ;

: field-handler!  ( xt -- )
    \ xt must have this effect: ( addr u u' -- ), where addr u is the content of the field and u' is the field#
    to field-handler  ; 

: rest-of-rec>list  ( -- addr )
    \ can be called from the new-line-handler, for example, when a trigger indicating a range start is encountered
    \ addr is the head of the new list; the list should be freed with: addr -rec-list
    0 to item-counter  0 to list-head  false (rest-of-rec>list)  list-head reverse-list  0 list-head free-list  ;

: -rec-list  ( addr -- )  ['] -cstring swap free-list  ;

: process-csv  ( fid -- )  \ fid is the csv file id
    with-csv-file-id  false (process-csv)  ;

privatize


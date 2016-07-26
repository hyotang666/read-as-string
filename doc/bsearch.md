# [Function] BSEARCH

## Syntax:

(BSEARCH item sorted-vector &KEY key test start end compare default) => result

## Arguments and Values:

item := any lisp object.

sorted-vector := SIMPLE-VECTOR which already sorted.

key := FUNCTION which applies each element of SORTED-VECTOR. Same with sequence function's (e.g. FIND) :KEY parameter.

test := FUNCTION which is used for ITEM and element of SORTED-VECTOR is same or not. Same with sequence function's (e.g. FIND) :TEST parameter.

start := (MOD #.ARRAY-TOTAL-SIZE-LIMIT) Same with sequence function's (e.g. FIND) :START parameter.

end := (MOD #.ARRAY-TOTAL-SIZE-LIMIT) Same with sequence function's (e.g. FIND) :END parameter.

compare := FUNCTION Same with SORT's second argument.

default := any lisp object. This will be returned when ITEM is not found in SORTED-VECTOR.

result := any lisp object.

## Description:

Almost same with FIND, but doing binary search.

SORTED-VECTOR must be sorted.

## See Also:

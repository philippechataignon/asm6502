 ; insertion sort of (up to) 256 8 bit elements
 ; orders them lowest to highest, bottom to top
 ; the list must be page aligned
 ; list =   address of first element
 ; top =    location containing the location in the
 ;          page of the top of the list = number of
 ;          elements - 1 (list is page aligned, zero
 ;          based, first element is 0)
 ; save_y = location to save where we are in the
 ;          list while we insert an out of place
 ;          element
 ; starts at the top of the list and scans downward
 ; looking for an out of place element ie a larger
 ; element under a smaller element. when we
 ; find one out of place we save it's location,
 ; stash it in X and scan upward bumping elements
 ; down as we go till we find where the current
 ; element goes ie till we either find an element >=
 ; to the the one we want to insert or untill we reach
 ; the top of the list. Then we go back to scanning
 ; downward for out of place elements picking up
 ; where we left off at the saved location

list=$1000
top=$fa
save_y=$fb

* = $f00

INSERTION_SORT
 ldy top
 beq DONE          ; if the last is the first it's already sorted
 bne ENTER_NEXT

INSERT
 sty save_y        ; remember where we were while we insert
 ldx list,y        ; get the current element out of the way in X

FIND_LOOP
 lda list+1,y      ; bump one down
 sta list,y
 iny
 cpy top           ; are we at the top of the list?
 beq PLACE         ; if yes, then go put current element in place
 txa               ; else get the current element in to A
 cmp list+1,y      ; to compare
 beq PLACE2        ; if = this is the place and the current
                   ; element is already in A

 bcs FIND_LOOP     ; if the current element is > go bump another
                   ; down

PLACE
 txa               ; get the current element in to A to place it
PLACE2             ; come here to place it if it's already in A
 sta list,y

 ldy save_y        ; pick up scanning down where we left off
 beq DONE          ; if we're at bottom we're done
 dey

NEXT
 lda list+1,y
 cmp list,y        ; if the current element > the one above
 bcc INSERT        ; go insert it in the right place

ENTER_NEXT
 dey
 bne NEXT

 lda list+1,y
 cmp list,y        ; still need to check the bottom element
 bcc INSERT

DONE
 rts

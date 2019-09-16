; All functions are done by recursion.
; This is primarily a recursion practice/exercise, and getting comfortable with Common Lisp syntax.
;
; Dot-product function
(defun dot-product (x y)
  ;check for x-or-y-equals-0 case
  (if (or (null x) (null y))
      0
    ;calculate the beginning of the elements in the list of x and y
    (+ (* (car x) (car y))
       ;call function again for the rest of the elements in the list until NIL
       (dot-product (cdr x) (cdr y)))
    )
)

; Counting number function
; Increments by one for all elements that are numbers in the list
(defun count-number (OneList)
  ;cond is an if-else-if conditional keyword that checks for a true case. The first parameter must be a boolean value, and the
  ;second parameter is for the statement
  (cond
   ;check for base case
   ((null OneList) 0)
   ;checks the first element in the list
   ;if it is a number, add one to the counter
   ((numberp (car OneList)) (1+ (count-number (cdr OneList))))
   ;otherwise, skips the first element and checks the rest of the list by calling it again
   (t (count-number (cdr OneList)))))

; Function that prints out a list of T's based on the value given
(defun new-list (x)
  ;check for base case
  (if (= x 0)
      ;print out nothing (values was the only thing I can find to literally print nothing)
      (values)
    ;adds T to a list and calls the function again
      (append '(t) (new-list (- x 1)))
      )
  )

; allLength function counts every element within a list
; unlike Length(), this one counts nested functions as well
; for every element, even NIL, it will increment the counter by one

(defun allLength (list)
  ;check the base case
  (cond ((null list) 0)
        ;check for first element, is this a nested list or a non-nested list? if non-nested, add one and check the rest
        ((atom (first list)) (+ 1 (allLength (rest list))))
        ;if nested check the nested list and the remaining list that isn't nested
        (t (+ (allLength(first list)) (allLength(rest list))))
        )
  )
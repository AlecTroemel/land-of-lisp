; Chapter 6.5: Lambda a function so important it deserves its own chapter
; lambda is creating a function without a name
; functions are first class values in lisp
(lambda (n) (/ n 2)) ; returns a function

; halve all values in a list
(mapcar (lambda (n) (/ n 2)) '(2 4 6))

; lambda is a macro
; lambda is also the only real command in lisp! (from lambda calculus)






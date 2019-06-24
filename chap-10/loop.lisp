;; sum is not very "lispy", its "hacky"
;; loop is a macro
(loop for i below 5 sum i) ;; sums 0 - 4
(loop for i from 5 to 10
   sum i)
(loop for i in '(100 20 3)
   sum i)
(loop for i below 5
   do (print i))
(loop for i below 10
   when (oddp i)
   sum i)
(loop for i from 0 ;; this will go to infinity without a break!
   do (print i)
   when (= i 5)
   return 'falafel)

;; loop can collect/map
(loop for i in '(2 3 4 5 6)
     collect (* i i)) ;; square everything

;; you can have multiple for's
;; increments both at the same time, NOT nested
(loop for x below 10
   for y below 10
   collect (+ x y))

;; you can nest loop blocks
(loop for x below 10
   collect (loop for y below 10
              collect (+ x y)))

;; clean eay to track index of items in list
(loop for i from 0
   for day in '(monday tuesday wednesday thursday friday saturday sunday)
   collect (cons i day))

;; check out page 200-201 for the "periodic table of the loop macro"
;; there is A LOT of things loop can do D:

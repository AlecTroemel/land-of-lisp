(setf foo '(A B C))
(setf (second foo) 'Z)
foo ;; (A Z C)

;; how does lisp know where second foo came from??
;; does this through "generalized reference"
;; setf is pretty powerful!
(setf foo (make-array 4))
(setf (aref foo 2) '(X Y Z)) ;; #(NIL NIL (X Y Z) NIL)
(setf (car (aref foo 2)) (make-hash-table))
(setf (gethash 'zoink (car (aref foo 2))) 5) ;; #(NIL NIL (#S(HASH-TABLE (ZOINK . 5)) Y Z) NIL)


;; some nice generic functions
(find-if #'numberp '(a b 5 d)) ;; => 5
(count #\s "mississippi") ;; => 4
(position #\4 "2kewl4sldnfskl") ;; => 5
(some #'numberp '(a b 5 d)) ;; => t
(every #'numberp '(a b 5 d)) ;; => NIL
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)
;; this can sum lists or array's
(defun sum (lst)
  (reduce #'+ lst))
(sum '(1 2 3))
(sum (make-array 5 :initial-contents '(1 2 3 4 4)))
(sum "blahblah")

;; map function works on all sequences
(map 'list
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this is a string")

;; sub sequence
(subseq "america" 2 6)

(sort '(5 8 2 4 9 3 9) #'<)

;; type checking functions
(numberp 5) ;; T
(numberp "hi") ;; NIL
;; arrayp characterp consp functionp has-table-p listp stringp symbolp

;; this is a bad idea, since things get slow and complicated quickly
(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b)) (append a b))))
(add 3 4)
(add '(a b ) '(c d))

;; defmethod allowys multiple definitions of same function
;; for different types
(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))
;; defmethod can be used with defstruct for a simple OOP systen

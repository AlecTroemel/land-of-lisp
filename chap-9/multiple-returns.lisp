;; lisp can return multiple things at once
(round 2.4) ;; returns 2; 0.4

;; create multiple returns with the 'values' function
(defun foo ()
  (values 3 7))

;; First value is more important
(+ (foo) 5) ;; -> 8

;; to get and use both values
(multiple-value-bind (a b) (foo) (* a b)) ;; -> 21

;; anything you can do with multiple values you can do by just returning a list
;; newer lisps dont even support it
;; but it can create more optimized and cleaner code

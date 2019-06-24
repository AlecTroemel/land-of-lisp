(cons 1 (cons 2 (cons 3 nil)))
; evals to (1 2 3)

(cons 1 (cons 2 3))
; evals to (1 2 . 3)
; the dot means that the list doesnt end in nil, but instead with 3
; any list that does not end in nil is called a dotted list

; one use for these are pairs
; these are good for 2d vectors or key value pairs
(cons 2 3) ; (2 . 3)

; circular lists are a thing
; should run this before so list knows to print them the right way
(setf *print-circle* t)

(defparameter foo '(1 2 3))

(setf (cdddr foo) foo)

; association lists (alist): list of key value pairs
(defparameter *drink-order* '((bill . double-expresso)
			      (lisa . small-drip-coffee)
			      (john . medium-lettee)))

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
; assoc gets the first pair with the matching key
(assoc 'lisa *drink-order*)

; adds a new pair to the front of the list
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

; assocs are great, but they're not very efficient

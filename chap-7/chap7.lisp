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
(setf *print-circle * t)

(defparameter foo '(1 2 3))
(setf (cdddr foo) foo)

; association lists (alist): list of key value pairs
(defparameter *drink-order* '((bill . double-expresso)
			      (lisa . small-drip-coffee)
			      (john . medium-lettee)))

; assoc gets the first pair with the matching key
(assoc 'lisa *drink-order*)

; adds a new pair to the front of the list
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

; assocs are great, but they're not very efficient


;; converting the wizard game into dot file
(defparameter *wizard-nodes* '(
 	(living-room (you are in the living room.
 		a wizard is snoring loudly on the couch.))
 	(garden (you are in the garden.
 		there is a well in fron of you.))
 	(attic (you are in the attic.
 		there is a gient welding torch in the corner.))))

(defparameter *wizard-edges* '(
	(living-room
		(garden west door)
		(attic upstairs ladder))
	(garden
		(living-room east door))
	(attic
		(living-room downstairs ladder))))

;; this functions converts names to match the Dot file convention
;; only letters digits and underscores
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; digit-char-p tells if a char is a number
(substitute-if #\e #'digit-char-p "I'm a l33t hack3r")
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8 9))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\";"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; thunks are functions that take no arguments
;; its common to print stuff to string/console
;; wrap in a thunk
;; then pass that thunk into a function that uses the side effects of the thunk
;; redirect the results to some other location
;; this makes debugging nice
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thumk))
  (ext:shell (concatenate 'string "dot -Tpng -O" fname)))

(with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "hello file!" my-stream))

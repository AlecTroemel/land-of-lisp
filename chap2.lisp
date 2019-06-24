; Chapter 2 Creting your first lisp program


; simple guess my number game 1 - 100-

; global variables
(defparameter *small* 1)
(defparameter *big* 100)
; (defvar *big* 100) ; is inmutatable 

; functions
; ash (arithmatic shift function) bit shift right or left (1 or -1 as second param)
; ash basically halves or doubles a number
(defun guess-my-number ()
	(ash (+ *small* *big*) -1))

; 1- is a function that returns 1 less than the param
(defun smaller ()
	(setf *big* (1- (guess-my-number)))
	(guess-my-number))
	
(defun bigger ()
	(setf *small* (1+ (guess-my-number)))
	(guess-my-number))

(defun start-over ()
	(setf *small* 1)
	(setf *big* 100)
	(guess-my-number))



; Local variables
; use "let" function
(defun local-variables ()
	(let ((a 5) 
		  (b 6))
		(+ a b)))

; local functions
; use "flet" (function let)
(defun local-function ()
	(flet ((f (n) 
			  (+ n 10))
		   (g (n) 
		   	  (- n 3)))
	(g (f 5))))

; labels is like flet exept you can use local functions in other local function
; can also do recursive calls this way
(defun local-labels-function (c)
	(labels ((a (n) 
			    (+ n 10))
		     (b (n) 
		   	    (- (a n) 3)))
	(b c)))
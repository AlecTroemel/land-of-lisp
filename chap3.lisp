; Chapter 3: Exploring the syntax of lisp code

; everything is lisp is a list
(defun square (n)
	(* n n))

; simbles are the basic building blocks of lisp
; case insensitive
(eq 'foo 'FoO)  ; returns true

; lisp has integer and floating point numbers
(expt 53 53)

(/ 4 6)  ; returns 2/3
(/ 4.0 6) ; returns 0.666667

; lisp has strings
(princ "Hello world \"escape chars\"")

; lisp has 2 ways of reading code

	; code mode
		; default of repl
		; needs to read a form (list with function at beginning)

	; data mode
		; not tried to be executed
		; denoted by single quote '
		; this is called "quoting"
		'(expt 3 4)


; Cons Cells (thing1 . thing2)
	; hold everything together. is like a linked list
	; think of it like 2 connected boxes, each pointing to something
	; lists are just connected cons cells
	
	; 3 basic functions for manipulating cons cells

		; cons: links 2 pieces of data together. "Consing" something
		(cons 'chicken 'cat) ; returns (CHICKEN . CAT) 
		; the dot tells us its a cons cell
		(cons 'chicken 'nil)
		(cons 'chicken ()) ; retuns (CHICKEN)
		; nil terminates a list
		; nil and () can be used interchangably
		(cons 'pork '(beef chicken)) ; returns (PORK BEEF CHICKEN)
		; whats really happening
		(cons 'pork (cons 'beef (cons 'chicken)))
		; chain of nested consing = list

		; car: get first part of cons cell
		(car '(pork beef chicken)) ; returns PORK

		; cdr: get second part of cons cell
		(cdr '(pork beef chicken)) ; returns (BEEF CHICKEN)

		; cadr is like (car (cdr)), gets second item in a list
		(cadr '(pork beef chicken)) ; returns BEEF

; List function
(list 'pork 'beef 'chicken) ; returns (PORK BEEF CHICKEN)

; lists can contain other lists
'(cat (duck bat) ant)

; ant c*r up to 4 levels deep exists already in lisp!!


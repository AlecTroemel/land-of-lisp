; Chapter 5: Building a text game engine

; var containing list of locations as an association list (alist)
(defparameter *nodes* '(
 	(living-room (you are in the living room. 
 		a wizard is snoring loudly on the couch.))
 	(garden (you are in the garden.
 		there is a well in fron of you.))
 	(attic (you are in the attic.
 		there is a gient welding torch in the corner.))))

; use assoc to find items in an alist
; this is writen in a purely functional way, only uses variables
; provided to the function, and will only return a value
(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

(defparameter *edges* '(
	(living-room 
		(garden west door)
		(attic upstairs ladder))
	(garden 
		(living-room east door))
	(attic
		(living-room downstairs ladder))))

; quasiquoting is enabled using backtick ` when switching to data mode
; with backtick you can unquote using comma character ,
(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))

; mapcar takes a function and a list, then applies the function
; to every item in the list

; #, is shorthand for function operator
; function names and variable names are in diferent namespacesv  

; append joins several lists into one list
; apply operator pretends that the items in the list are separate objects
(defun describe-paths (location edges)
	(apply #'append 
		(mapcar #'describe-path 
			(cdr (assoc location edges)))))

; objects
(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '(
	(whiskey living-room)
	(bucket living-room)
	(chain garden)
	(frog garden)))

; labels command allows the def of local functions
; at-loc-p has p at end for predicate since it returns t or nil
(defun objects-at (loc objs obj-locs)
	(labels ((at-loc-p (obj)
		(eq (cadr (assoc obj obj-locs)) loc)))
	(remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
	(labels ((describe-obj (obj)
		`(you see a ,obj on the floor.)))
	(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; player location, start at living room
(defparameter *location* 'living-room)

; main look command, thing players will use
; NOT functional since the same command can give differeny results
(defun look ()
	(append 
		(describe-location *location* *nodes*)
		(describe-paths *location* *edges*)
		(describe-objects *location* *objects* *object-locations*)))


; Walk function (NOT functional)
; find searches for a list of items and returns that item
; keyword parameters begin with a colon, are special function parameters at end of call
	; they follow format :key_name value
(defun walk (direction)
	(let ((next (find direction ; set variable
		(cdr (assoc *location* *edges*))
		:key #'cadr)))
	(if next
		(progn (setf *location* (car next))
			(look))
		'(you cannot go that way.))))

; pickup user function
; member checks to see if a particular item is found in a list of items
; push is like a list push, convenience around setf
; we're never really removing anything from object-locations,
; but since assoc grabs the first thing it finds, its like we're mutating the list
; this also preserves history...at cost of memory
(defun pickup (object)
	(cond (
		(member object 
			(objects-at *location* *objects* *object-locations*))
		(push (list object 'body) *object-locations*)
			`(you are now carrying the ,object))
		(t '(you cannot get that.))))

; invintory user function 

(defun invintory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))

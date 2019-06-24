; Chapter 4: Making Decisions with Conditions

; empty list is false
(if '()
	'i-am-true 
	'i-am-false) ; returns I-AM-FALSE

; this makes recursion nice
(defun my-length2 (list)
	(if list 
		(1+ (my-length (cdr list)))
		0))

; an empty list is the ONLY thing considered false
; other things are disguesed as ()
; '() == () == 'nil == nil

; if is a special form, which gives it special privoleges
; so we can do stuff like this (divide by 0 isnt evaluated)
(if (oddp 5)
	'odd-number
	(/ 1 0))

; if can only do 1 thing, progn is a way around this 
(defvar *number-was-odd* nil)

(if (oddp 5)
	(progn (setf *number-was-odd* t)
		'odd-number)
	'even-number)

; when and unless have implicit progn
; everythin in enclosed parenths is evaled when true
; trade off is they arnt switches like if, its all or nothing
(when (oddp 5)
	(setf *number-was-odd* t)
		'odd-number)

; cond has it all...with lots of ()
; the last condition is t for true
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
	(cond ((eq person 'henry) 
				(setf *arch-enemy* 'stupid-lisp-alien)
				'(curse you lisp alien - you ate my pudding))
		  ((eq person 'johnny) 
		  		(setf *arch-enemy* 'useless-old-johnny)
		  		'(I hope you choked on the pudding johnny))
		  (t '(why you eat my pudding stranger?))
	)
)

; case supply a value to compare against


(defun pudding-eater-case (person)
	(case person
		((henry)
			(setf *arch-enemy* 'stupid-lisp-alien)
			'(curse you lisp alien - you ate my pudding))
		((johnny)
			(setf *arch-enemy* 'useless-old-johnny)
			'(I hope you choked on the pudding johnny))
		(otherwise 
			'(why you eat my pudding stranger?))))

; boolean conditions and, or
	(and (oddp 5) (oddp 7) (oddp 9)) 
	(or (oddp 4) (oddp 6) (oddp 9)) 

	; can be used for condition behavior
	; this is because lisp uses short circuit boolean logic
	(defvar *is-it-even* nil)
	(or (oddp 4) (setf *is-it-even* t))

	; these functions are all the same
	(if *file-modified*
		(if (ask-user-about-saving)
			(save-file)))

	(and *file-modified* (ask-user-about-saving) (save-file))

	(if (and *file-modified* (ask-user-about-saving))
		(save-file))

; always ask what else could lisp return other then t?
(member 1 '(3 4 1 5)) ; returns (1 5) 
; thinking about cons explains that 
; this case stop it from just returning the 1 member 
(member nil '(3 4 nil 5))

; these type of returns are useful
(find-if #'oddp '(1 2 3 4 5)) ; only finds the first value

; .. nil produces the one problem
(find-if #'null '(2 3 nil 5))

; equal comparason isn't so beutifull
	; equal, eql, eq, = , string-equal, equalp
	; rule of thumb
		; 1. use EQ to compare symbols
			; its simple and fast
		; 2. use EQUAL for everything else
			; looks to see if 2 things are isomorphic (look the same)

	; eql: like eq, but also can do numbers and chars
	; equalp: like equal, with some more edge cases
		; strings with different caps
		(equalp "Bob smith" "BOB SMITH") ; returns true
		; ints an floats
		(equalp 0 0.0) ; returns true

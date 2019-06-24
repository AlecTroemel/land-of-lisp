; Chapter 6: Reading and Printing in lisp

; print put's stuff to the console
(print "test")

; prin1 is exactly the same as print, just without the newline
(progn 
	(prin1 "all")
	(prin1 "on")
	(prin1 "one")
	(prin1 "line"))

; read 
(defun say-hello ()
	(print "please type your name:")
	(let ((name (read)))
		(print "nice to meet you,")
		(print name)))

; print and read are good for pretty much every data type

(defun add-five ()
	(print "please enter a number:")
	(let ((num (read)))
		(print "when you add 5 you get,")
		(print (+ num 5))))

; literal characters
; some hidden ones are
	; #\newline #\tab #\space
(print '#\a)

; case sensative symbols can be made by |surrounding in a pipe.|
; can even use period!

; princ is for human readable print
(princ '"foo") ; -> foo

n(progn 
	(princ "this sentence will be interrupted")
	(princ #\newline)
	(princ "by the newline character."))

; read-line just reads whatever you type as a string

(defun say-hello-2 ()
	(printc "please type your name:")
	(let ((name (read-line)))
		(printc "nice to meet you,")
		(printc name)))

; eval can execute code that is saved in data mode
; dont use it too much though... sometimes macro can be better
(defparameter *foo* '(+ 1 2))
(eval *foo*)

; creating own repl is really easy
; loop simply loops forever
(defun game-repl ()
  (loop (print (eval (read)))))

;  we can have our game loop do a little more
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

; game read formats our input
; 'foo is the same as (quote foo)
; this repl isnt perfect, consider entering "(foo "
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; we only want certain commands to be executed
(defparameter *allowed-commands* '(look walk pickup inventory))
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i dont know that command.)))

; game print formats our code, its the most complex thing so far!
; its best to storea data in the most comfertable way, then translate it at the end
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  rest (cdr lst)))
    (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	  ((member item '(#\! #\? #\.) (cons item (tweak-text rest t lit)))
	  ((eql item #\") (tweak-text rest caps (not lit)))
	  (lit (cons item (tweak-text rest nil lit)))
	  (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	  (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; coerce converts string to list of chars and the other way around
; fresh-line makes sure the next thing to print will be on a new line
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))







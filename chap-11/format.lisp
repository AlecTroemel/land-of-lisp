
;; first param:
;;     nil: just create a string
;;       t: print to console
;;  stream: output to stream
(format t "add onion rings for only ~$ dollars more!" 1.5)
(princ (reverse (format nil "add onion rings for only ~$ dollars more!" 1.5)))

;; "~$" is the "control string", they all begin with ~
;; $: monetary floating-point
;; s: same as prin1 (quotes)
;; a: same as princ (no quotes)
;; you can add right padding  like this ~10a
;; left padding like this ~10@a

;; control strings can have multiple parameters
;; add spaces in sets of 3, until 10 (in this case 9 spaces total). This is rarely used
(format nil "add onion rings for only ~10,3a dollars more!" "foo")
;; parameter 3 is the exact number of spaces to add, regarless of length of final value
(format nil "i am printing ~,,4a in the middle of this sentence." "foo")
;; param 4 is the padding char
(format nil "i am printing ~,,4,'!a in the middle of this sentence." "foo")
(format nil "i am printing ~,,4,'!@a in the middle of this sentence." "foo")


;; Formatting Numbers
(format nil "the number 1000 in hex is ~x" 1000)
(format nil "the number 1000 in binary is ~b" 1000)
(format nil "the number 1000 in explicit base10 is ~d" 1000)
(format nil "number with commas in them ~:d" 1000000)
(format nil "padding just like earlier ~10,'xd" 100000)

;; Formatting Floating Points
(format nil "PI with 4 characters ~4f" 3.141593)
(format nil "PI rounded to 4 decimals ~,4f" pi)
(format nil "floating  can be scaled with the 3rd parm ~,,2f" pi)

;; Formatting multiple lines
;;
(progn (princ 22)
       (terpri) ;; start new line
       (princ 33))
(progn (princ 22)
       (fresh-line) ;; start new line IF NEEDED
       (princ 33))
(progn (format nil "this is on one line ~%") ;; format version of terpri
       (format nil "~%this is on another line"))
(progn (format nil "this is on one line ~&") ;; format version of fresh-line
       (format nil "~&this is on another line"))
(format t "this will print ~5%on  two lines spread far apart")

;; Justifying Output
;;
(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangeroo")))

;; ~t formats things into a table
(loop repeat 10
      do (format t "~5t~a ~15t~a ~25t~a~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; to get the animals equally spaced apart per line
;; pg 229 for full deconstruction of the str
(loop repeat 10
      do (format t "~30<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; neatly centered column
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

;; :@ can be used for some interesting formatting
(loop repeat 10
      do (format t "~30:@<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; but that looks wavy, to get centered columns
(loop repeat 10
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; Iterating through list with control sequences
;;
;;the stuff between ~{ and ~} are treated almost like the innerds of a for loop, just pass in a list
(defparameter *animals* (loop repeat 10 collect (random-animal)))
(format t "~{I see a ~a!~%~}" *animals*)
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*) ;; can aslo grab multiple items in 1 iteration!

;; One last crazy way to format tables
(format t "|~{~<|~%|~,33:;~2d ~>~}|~%" (loop for x below 100 collect x))

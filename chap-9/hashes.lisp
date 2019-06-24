;; Hashes are like fast alists
(defparameter x (make-hash-table)) ;; #S(HASH-TABLE ...)
(gethash 'yup x) ;; return 2 things, value and whether or not the key was found
;; to set
(setf (gethash 'yup x) '25)

(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-expresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(gethash 'lisa *drink-order*) ;; -> 'small-drip-coffee

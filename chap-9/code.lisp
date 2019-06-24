;; track time it takse to do something 100 times
(time (dotimes (i 100) (* 10 10)))

;; more effecient wumpus functions
;; converts edge list into hash table
(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x) ;; mapc is like each function, only side effects
            (let ((node (car x)))
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab)) ;; return tab

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited))

;; structures
;; think OOP objects and properties
;; properties are called "slots"
(defstruct person
  name
  age
  waist-size
  favorite-color)

;; defstruct automatically generates a make-NAME function
(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

;; it alsp autogens functions for getting slots
(person-age *bob*)
(setf (person-age *bob*) 36)

;; can create person from printed rep
(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))

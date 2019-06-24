;; simulation of evolving creatures
(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10)) ;; (x y width height)
(defparameter *plant-energy* 80)

;; Plants
;; we'll be using cons pairs for the key, which requires "equal" instead of the
;; default "eq". We're really hacking a set datatype
(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let ((pos (cons
              (+ left (random width))
              (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

;; adds 1 plant to the jungle, and 1 outside the jungle. Since the jungle is
;; small, it will naturally become more dense!
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;; Animals
;; - x,y coords in map
;; - energy is "days remaining"
;; - dir: direction of next x,y position. 0-7 clockwise starting in top-right
;; - genes: 8 positive ints, represent licklyhood to turn in that dir
(defstruct animal x y energy dir genes)

;; seed the world with our "adam" animal
;; we never need to search our animal list, so lists are performant enough
(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8
                               collecting (1+ (random 10))))))

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x ;; wrap around world
                                    (cond ((and (>= dir 2) (< dir 5)) 1) ;; east
                                          ((or (= dir 1) (= dir 5)) 0) ;; north/south
                                          (t -1))) ;; west
                                 *width*))
    (setf (animal-y animal) (mod (+ y ;; wrap around world
                                    (cond ((and (>= dir 0) (< dir 3)) -1) ;; north
                                          ((and (>= dir 4) (< dir 7)) 1) ;; south
                                          (t 0))) ;; east/west
                                 *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal))))) ;; random number within sum of genes
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod ;; wrap turning around
             (+ (animal-dir animal) (angle (animal-genes animal) x))
             8)))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

;; animals reproduce asexualy
(defparameter *reproduction-energy* 200)
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1)) ;; half energy
      (let ((animal-nu (copy-structure animal)) ;; shallow copy
            (genes (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes) ;; apply some mutation to the genes
              (max 1 (+ (nth mutation genes) (random 3) -1))) ;; maybe add +/- 1
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

;; Simulation
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal) ;; remove dead animals D:
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal) ;; remember mapc is like "each"
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-line (y)
  (loop for x below *width*
     do (princ (cond ((some (lambda (animal) ;; draw animal
                              (and (= (animal-x animal) x)
                                   (= (animal-y animal) y)))
                            *animals*)
                      #\M)
                     ((gethash (cons x y) *plants*) #\*) ;; draw plant
                     (t #\space))))) ;; else empty space

(defun draw-world ()
  (loop for y below *height*
     do (progn (fresh-line)
               (princ "|")
               (draw-line y)
               (princ "|"))))

;; user interface
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i below x ;; if input is valid number
                      do (update-world)
                      ;; print update dot on large runs so we know the computer isnt frozen
                      if (zerop (mod i 1000)) do (princ #\.))
                   (update-world)) ;; if input isnt valid number, just run once
               (evolution))))))

(evolution) ;; start simulation!

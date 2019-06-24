(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect
                        (edge-pair (random-node) (random-node)))))

(make-edge-list)

;; some  loop examples
(loop repeat 10 collect 1)
(loop for n from 1 to 10 collect n)
(loop for n from 1 to 10 collect (+ 100 n)) ;; can put any code after the collect

;; we need to connect islands

;; find all edges that start from a node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

;; generate list of all nodes connected to the given node (even through multiple steps)
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                          (let* ((connected (get-connected (car nodes) edge-list))
                                 (unconnected (set-difference nodes connected)))
                            (push connected islands)
                            (when unconnected
                              (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)                                  ;; islands length > 1
    (append (edge-pair (caar islands) (caadr islands)) ;; merge island 1 with island 2
            (connect-with-bridges (cdr islands)))))    ;; recursively do the same thing with the rest

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; Build the final edges for congestion City!
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

;; converts ((1 . 2) (2 . 1) (2 . 3) (3 . 2))
;; to ((1 (2)) (2 (1 3)) (3 (2)))
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                           (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun make-city-edges ()
  (let*
      ((nodes
        (loop for i from 1 to *node-num* collect i))
       (edge-list
        (connect-all-islands nodes (make-edge-list)))
       (cops
        (remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; let* lets you use previously defined variables
;;
;; this fails
;; (let ((a 5)
;;       (b (+ a 2)))
;;   b)

;; ;; but this works!
;; (let* ((a 5)
;;       (b (+ a 2)))
;;   b)

(defun neighbors (node edge-alist)
  (mapcar
   #'car ;; we only care about the neighbor node itself, not its edges
   (cdr (assoc node edge-alist)))) ;; all edges connected to node

;; check if a and b are only 1 apart
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x) ;; go another layer in
              (within-one x b edge-alist))
            (neighbors a edge-alist))))


;; time to build the final map of our city
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
       (glow-worms (loop for i below *worm-num*
                      collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       ;; wumpus hints
                        (cond ((eql n wumpus) '(wumpus))
                              ((within-two n wumpus edge-alist) '(blood)))
                        ;; glowworm hints
                        (cond ((member n glow-worms) '(glow-worm))
                              ((some (lambda (worm) (within-one n worm edge-alist)) glow-worms)
                               '(lights!)))
                        ;; police hints
                        (when (some #'cdr (cdr (assoc n edge-alist)))
                          '(sirens!))))))


;; fyi if there are no open spots this will loop forever
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*)) ;; if there's something there
        (find-empty-node) ;; try again
        x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


;; time to draw city from partial knowledge
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node)
                             (mapcar #'car
                                     (cdr (assoc node
                                                 *congestion-city-edges))))
                           *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

;; mapcan: values generated are appended together
;;
;; (defun ingredients (order)
;;   (mapcan (lambda (burger)
;;             (case burger
;;               (single '(patty))
;;               (double '(patty patty))
;;               (double-cheese '(patty patty cheese))))
;;           order))
;; (ingredients '(single double-cheese double))


;; lets draw the known parts of the map
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

;; walking around town
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* node)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "you ran into the cops. Game Over."))
          ((member 'wompus node) (if charging
                                     (princ "you found the Wumpus!")
                                     (princ "you ran into the Wumpus")))
          ((charging) (princ "you waster your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "you ran into the glow worm gang! youre now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*))))) ;; legal directions player can move
    (if edge
        (handle-new-place edge pos charging)
        (princ "that location does not exist!"))))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

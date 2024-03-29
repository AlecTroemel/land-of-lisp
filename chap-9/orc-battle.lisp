;; your a knight surrounded by 12 orcs in a fight to the death!
(defparameter *player-health* nil)   ;; game over at 0
(defparameter *player-agility* nil) ;; # of moves in a turn
(defparameter *player-strength* nil) ;; power of moves

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;; helpers
;; random number between 1-n
(defun randval (n)
  ;; random give number from 0 to (n-1)
  (1+ (random (max 1 n))))

;; the generic monster
(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x) ;; decriment
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
      (progn (princ "you hit the ")
             (princ (type-of m))
             (princ ", knocking of ")
             (princ x)
             (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

;; Specific monsters
;; orc: random level clubs
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)
(defmethod monster-show ((m orc))
  (princ "a wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))
(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;; hydra: gains +! health after attacking, does number of heads attack
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)
(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))
(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "the coprse of the fully decapitated and decapacitated hydra falls to the floor")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of the hydras heads! "))))
(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;; slime mold: lowers agility
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)
(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))
(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking awat a health points! ")
      (decf *player-health*))))

;; brigand: will try to neutralize your best assets, has consistent power
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)
(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, doing 2 damage! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, takeing 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))


;; monster healper functions
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
               (progn (princ "health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m)))) ;; this function will do most of the work
         *monsters*)))

;; player healper functions
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princ "monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "that is not a valid monster number")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead")
                     (pick-monster))
              m)))))

(defun player-attack ()
  (fresh-line)
  (princ "attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    ;; it 1 monster
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    ;; weaker then stab, but hit 2 monsters
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "your double swing has an attack of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monster-dead)
           (monster-hit (pick-monster) x))))
    ;; attack random foes multiple ties
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))


(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    ;; convert agility into a smaller number
    ;; dotimes runs code n times
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

;; main start function
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game over."))
  (when (monsters-dead)
    (princ "Congradulations! You have vanquished all your foes.")))

(orc-battle)

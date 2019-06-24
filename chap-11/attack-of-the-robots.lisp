;; Robots have taken over the world, its up to you to defeat them!
;; robots move towards the player, but collide and create "scrap"
;; which is deadly to other robots
;; use these characters to move in a grid
;; q w e
;; a   d
;; z x c
;;
;; use t to teleport
;;
;; This game uses a nightmarish abuse of loop and format to fit the
;; whole game into "a single page of code".
(defun robots ()
  (loop named main ;; naming the loop allows us to "return from" early
        with directions = '((q . -65) (w . -64) (e . -63) (a . -1) ;; character offset when board in 64 wide
                          (d . 1) (z . 63) (x . 64) (c . 65))
        for pos = 544
        then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave":)
                    (force-output)
                    (let* ((c (read))
                           (d (assoc c directions))) ;; assoc performs lookup
                      (cond (d (+ pos (cdr d)))
                            ((eq 't c) (random 1024)) ;; game baord is 64x16=1024, rand that gives random spot
                            ((eq 'l c) (return-from main 'bye)) ;; player wants to win game
                            (t pos))))
        for monsters = (loop repeat 10 collect (random 1024)) ;; change the 10 for more monsters
        then (loop for mpos in monsters
                   collect (if (> (count mpos monsters) 1)
                               mpos
                               (cdar (sort (loop for (k . d) in directions
                                                 for new-mpos = (+ mpos d)
                                                 collect (cons (+ (abs (- (mod new-mpos 64) ;; this all calculates the
                                                                          (mod pos 64)))    ;; "manhattan distance" to
                                                                  (abs (- (ash new-mpos -6) ;; the player.
                                                                          (ash pos -6))))   ;;
                                                               new-mpos))
                                           '<
                                           :key #'car))))
        when (loop for mpos in monsters ;; checks if all monsters are scrap
                   always (> (count mpos monsters) 1)) ;; more then 1 robot in spot means they're scrap
        return 'player-wins ;; if theyre all scrap, you win!
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|" ;; pay no mind to this...
                   (loop for p
                         below 1024 ;; loop through all board positions
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses)) ;; if player in same position as a monster robot
                                              ((> (count p monsters) 1) #\#) ;; draw scraps for double upped robots
                                              (t #\A))) ;; draw robot
                                       ((= p pos) #\@) ;; draw plater
                                       (t #\ )))))) ;; the space here is important

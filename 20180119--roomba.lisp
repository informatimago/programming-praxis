;; Roomba
;; 
;; January 19, 2018
;; 
;; A robot can move any number of steps in the four cardinal
;; directions. Given the robot’s initial position and a string of moves
;; given as, for instance, N3E5S2W6 (any of the four cardinal directions,
;; followed by any number of steps, as many commands as desired),
;; determine the ending position of the robot.
;; 
;; Your task is to write a program to determine the ending position of a
;; robot, given a starting position and a string of move commands. When
;; you are finished, you are welcome to read or run a suggested solution,
;; or to post your own solution or discuss the exercise in the comments
;; below.

;; [sourcecode lang= "css"]

(defun split-vector-on-indicator (vector indicator)
  "
RETURN: a list of subvector of vector, the vector is splited between
        elemtns a and b for which (indicator a b).
"
  (loop
    :with result := '()
    :with start := 0
    :for i :from 0 :below (1- (length vector))
    :do (when (funcall indicator (aref vector i) (aref vector (1+ i)))
          (push (subseq vector start (1+ i)) result)
          (setf start (1+ i)))
    :finally (return (nreverse (cons (subseq vector start) result)))))

(defun xor (a b)
  "Return A ⊻ B"
  (or (and a (not b)) (and (not a) b)))



;; with vectors:

(defun roomba (initial-position path)
  (loop
    :with position := (make-array 2 :initial-contents initial-position)
    :for (direction steps)
      :on (split-vector-on-indicator path (lambda (a b)
                                            (xor (digit-char-p a)
                                                 (digit-char-p b))))
    :by (function cddr)
    :for distance := (parse-integer steps)
    :do (map-into position
                  (lambda (x dx) (+ x (* dx distance)))
                  position
                  (ecase (intern direction)
                    (n #(0 +1))
                    (s #(0 -1))
                    (e #(+1 0))
                    (w #(-1 0))))
    :finally (return position)))

(assert (equalp (roomba #(0 0) "N3E5S2W6") #(-1 1)))
(assert (equalp (roomba #(0 0) "N10E10S10W10") #(0 0)))
(assert (equalp (roomba #(0 0) "W4S19W33N17E37N2") #(0 0)))



;; with complexes:

(defun roomba/c (initial-position path)
  (loop
    :for position := initial-position
      :then (+ position (* distance
                           (ecase (intern direction)
                             (n #c(0 +1))
                             (s #c(0 -1))
                             (e #c(+1 0))
                             (w #c(-1 0)))))    :for (direction steps)
      :on (split-vector-on-indicator path (lambda (a b)
                                            (xor (digit-char-p a)
                                                 (digit-char-p b))))
    :by (function cddr)
    :for distance := (parse-integer steps)
    :finally (return position)))

(assert (= (roomba/c #C(0 0) "N3E5S2W6")         #C(-1 1)))
(assert (= (roomba/c #C(0 0) "N10E10S10W10")     #C(0 0)))
(assert (= (roomba/c #C(0 0) "W4S19W33N17E37N2") #C(0 0)))

;; [/sourcecode]


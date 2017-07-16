;; Nuts And Bolts
;; July 14, 2017
;; Today’s exercise is an interview question from Microsoft:
;; 
;; You are given two bags, one containing bolts and the other containing
;; nuts, and you need to find the biggest bolt.. You may compare bolts to
;; nuts, to see which is larger, but you may not compare bolts to bolts
;; or nuts to nuts. Write a program to find the biggest bolt.
;; 
;; Your task is to write a program to find the biggest bolt. When you are
;; finished, you are welcome to read or run a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.

#|
[sourcecode lang="css"]
|#

(defun find-biggest-nut (nuts bolts)
  (loop
    :while (and (cdr nuts) bolts)
    :do (let ((bolt         (pop bolts))
              (bigger-nuts  '())
              (smaller-nuts '()))
          (dolist (nut nuts)
            (if (<= nut bolt)
                (push nut smaller-nuts)
                (push nut bigger-nuts)))
          (setf nuts (or bigger-nuts smaller-nuts)))
    :finally (return nuts)))


(defun set-equalp (a b)
  (and (subsetp a b)
       (subsetp b a)))

(progn
 (assert (set-equalp (find-biggest-nut '(1 2 3 4 5)   '(1 2 3 4))   '(5)))
 (assert (set-equalp (find-biggest-nut '(1 2 3 4 5)   '(2 4))       '(5)))
 (assert (set-equalp (find-biggest-nut '(1 2 3 4 5 6) '(2 4))       '(5 6)))
 (assert (set-equalp (find-biggest-nut '(1 2 3 4 5 6) '(1 2 3 4))   '(5 6)))
 (assert (set-equalp (find-biggest-nut '(1 2 3 4 5 6) '(1 2 3 4 7)) '(5 6)))
 :success)

#|
[/sourcecode]
|#


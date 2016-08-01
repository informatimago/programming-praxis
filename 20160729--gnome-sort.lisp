;; https://programmingpraxis.com/2016/07/29/gnome-sort/
;;
;; Gnome Sort
;; July 29, 2016
;; 
;; A garden gnome sorts flower pots by the following method:
;; 
;;     The gnome looks at the flower pot next to him and the flower pot
;;     just behind him. If they are correctly ordered, so the flower pot
;;     just behind him is smaller than the flower pot next to him, he
;;     steps one pot forward; otherwise, he swaps the two flower pots and
;;     steps one pot backward. If there is no flower pot just behind him
;;     (thus, he is at the start of the line of flower pots), he steps
;;     forward to the next pot. If there is not flower pot next to him
;;     (thus, he is at the end of the line of flower pots), he is done.
;; 
;; Your task is to implement a program that sorts by the gnome
;; method. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

(defun gnome-sort (mutable-vector lessp)
  (let ((gnome-position 0))
    (loop
      ;; (print mutable-vector)
      (cond
        ((zerop gnome-position)
         (incf gnome-position))
        ((= gnome-position (length mutable-vector))
         (return mutable-vector))
        ((funcall lessp
                  (aref mutable-vector (1- gnome-position))
                  (aref mutable-vector gnome-position))
         (incf gnome-position))
        (t
         (rotatef (aref mutable-vector (1- gnome-position))
                  (aref mutable-vector gnome-position))
         (decf gnome-position))))))

(gnome-sort (vector 1 10 2 9 3 7 4 8 5 6) (function <))
;; --> #(1 2 3 4 5 6 7 8 9 10)

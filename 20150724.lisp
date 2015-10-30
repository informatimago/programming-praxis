;; From: programmingpraxis <post@gwene.org>
;; Subject: One-Swappable Array
;; Newsgroups: gwene.com.yahoo.pipes.lisp
;; Date: Fri, 24 Jul 2015 11:00:28 +0200 (3 hours, 19 minutes, 38 seconds ago)
;; Message-ID: <x1-G6FSdGsHXfdUXG8ws4mDbk05nSM@gwene.org>
;; 
;; Today’s exercise helps somebody with their homework: Given an array
;; of unique integers, determine if it is possible to sort the array by
;; swapping two elements of the array. For instance, the array
;; [1,2,6,4,5,3,7] can be sorted by swapping 3 and 6, but there is no
;; way to sort the array [5,4,3,2,1] by swapping two of its elements.
;; You may use O(n) time, where the array has n integers, and constant
;; additional space. Your task is to write a program that determines if
;; an array can be sorted with a single swap. When you are finished, you
;; are welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.


(defun one-swappable-vector-p (vector)
  (case (length vector)
    ((0 1) nil)
    ((2) (< (aref vector 1)  (aref vector 0)))
    (otherwise
     (loop
       :with count := 0
       :with indices := (vector nil nil)
       :for i :below (1- (length vector))
       :when (< (aref vector (1+ i))  (aref vector i))
         :do (when (<= 2 count)
               (return nil))
             (setf (aref indices count) i)
             (incf count)
       :finally (return (if (= 2 count)
                            (let ((left  (aref indices 0))
                                  (right (1+ (aref indices 1))))
                              (and (if (plusp left)
                                       (<= (aref vector (1- left))  (aref vector right) (aref vector (1+ left)))
                                       (<=                          (aref vector right) (aref vector (1+ left))))
                                   (if (< (1+ right) (length vector))
                                       (<= (aref vector (1- right)) (aref vector left)  (aref vector (1+ right)))
                                       (<= (aref vector (1- right)) (aref vector left)))))
                            
                            (let ((left  (aref indices 0))
                                  (right (1+ (aref indices 0))))
                              (cond
                                ((zerop left)
                                 (<=                         (aref vector right) (aref vector left) (aref vector (1+ right))))
                                ((< (1+ right) (length vector))
                                 (<= (aref vector (1- left)) (aref vector right) (aref vector left) (aref vector (1+ right))))
                                (t
                                 (<= (aref vector (1- left)) (aref vector right) (aref vector left)))))))))))


(defun test/one-swappable-vector-p ()
  (assert (not (one-swappable-vector-p #(1))))
  (assert (one-swappable-vector-p #(2 1)))
  (assert (one-swappable-vector-p #(1 2 6 4 5 3 7)))
  (assert (one-swappable-vector-p #(1 2 3 5 4 6 7)))
  (assert (one-swappable-vector-p #(8 1 2 3 4 5 6 7 0)))
  (assert (not (one-swappable-vector-p #(1 2 6 4 5 3 7 8 9 0 10))))
  (assert (not (one-swappable-vector-p #(1 2))))
  (assert (not (one-swappable-vector-p #(5 4 3 2 1))))
  :success)

(test/one-swappable-vector-p)








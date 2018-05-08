;; An Array Exercise
;; 
;; May 8, 2018
;; 
;; We worked on linked lists in the previous exercise, so today we will
;; work on arrays:
;; 
;; 
;; 
;; Given an array of distinct integers, replace each element of the array
;; with its corresponding rank in the array. For instance, the input
;; array [10,8,15,12,6,20,1] is replaced by the output array
;; [4,3,6,5,2,7,1] because element 1 has rank 1, element 6 has rank 2,
;; element 8 has rank 3, element 10 has rank 4, element 12 has rank 5,
;; element 15 has rank 6, and element 20 has rank 7.
;; 
;; 
;; 
;; Your task is to write a program to replace array elements by their
;; rank. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.


[sourcecode lang="css"]

;; First idea:
;; - wrap the elements with their original position,
;; - sort the wrapping by element,
;; - replace the elements by their rank = position in the sorted vector,
;; - sort the wrapping by the original position,
;; - unwrap.
;; The principle is simple, and we obtain easily a correct solution:

(defun vector-ranks (vector)
  ;; This is O(nlogn+n+nlogn+n) = O(nlogn) in time,
  ;; and O(3n)=O(n) in space.
  ;; Original vector is not modified.
  (map 'vector (function cdr)
       (sort (map 'vector (let ((rank 0))
                            (lambda (entry)
                              (setf (cdr entry) (incf rank))
                              entry))
                  (sort (map 'vector (let ((position -1))
                                       (lambda (element) (cons (incf position) element)))
                             vector)
                        (function <)
                        :key (function cdr)))
             (function <)
             :key (function car))))

(assert (equalp (vector-ranks #(10 8 15 12 6 20 1))
                #(4 3 6 5 2 7 1)))

;; If you insist on mutation:
(defun program-to-replace-vector-elements-by-their-rank (vector)
  (replace vector (vector-ranks vector)))



;; Then, we can see that since we have the position in the sorted
;; wrapping, we can update directly the original vector without
;; re-sorting and unwrapping, thus obtaining an optimized solution:

(defun replace-vector-by-rank (vector)
  ;; This is O(nlogn+n) = O(nlogn) in time,
  ;; and O(n) in space.
  ;; Original vector is modified.
  (let ((sorted (sort (map 'vector (let ((position -1))
                                     (lambda (element) (cons (incf position) element)))
                           vector)
                      (function <)
                      :key (function cdr))))
    (loop
      :for rank :from 1
      :for (position) :across sorted
      :do (setf (aref vector position) rank))
    vector))

(assert (equalp (replace-vector-by-rank (vector 10 8 15 12 6 20 1))
                #(4 3 6 5 2 7 1)))

[/sourcecode]

;; Sorting By Frequency
;; January 5, 2018
;; 
;; We have another homework question today, as I continue clearing out my
;; backlog of people who sent me email asking for homework help last
;; fall:
;; 
;; You are given an array with duplicates, and are to sort the array by
;; decreasing frequency of elements. If two elements have the frequency,
;; sort them in increasing order of value. For instance, the input (2 3 5
;; 3 7 9 5 3 7) is sorted as (3 3 3 5 5 7 7 2 9).
;; 
;; Your task is to write a program that sorts by frequency. When you are
;; finished, you are welcome to read or run a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.



;; [sourcecode lang="css"]

;; A natural solution written in Common Lisp, would not care for
;; "stability" (conserving a specific order for the elements beyond
;; the sorted criteria).  To be able to handle big sequences in nlogn
;; time, we would use a hash-table to compute the "frequencies",
;; getting the following solution:

(defun sort-by-frequency (sequence lessp &key (key (function identity)))
  "
Sorts the elements in the SEQUENCE based on the frequency of their KEY.
LESSP shall be an order function on REAL.
"
  (flet ((compute-frequencies (sequence key)
           (let ((table (make-hash-table)))
             (map nil (lambda (item) (push item (gethash (funcall key item) table '())))
               sequence)
             table))
         (wrap (table)
           (let ((frequencies (make-array (hash-table-count table)))
                 (i -1))
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (setf (aref frequencies (incf i)) (cons (length v) v)))
                      table)
             frequencies))
         (sort-frequencies (frequencies lessp)
           (sort frequencies lessp :key (function car)))
         (unwrap (result frequencies)
           (if (listp result)
               (loop
                 :with current := result
                 :for (count . elements) :across frequencies
                 :do (replace current (reverse elements))
                     (setf current (nthcdr count current)))
               (loop
                 :with start := 0
                 :for (count . elements) :across frequencies
                 :do (replace result (reverse elements) :start1 start)
                     (incf start count)))
           result))
    (unwrap sequence
            (sort-frequencies (wrap (compute-frequencies sequence key)) lessp))))


(assert (equalp (sort-by-frequency (list 1 2 3 4 5 6 7)       '< :key (function oddp)) '(2 4 6 1 3 5 7)))
(assert (equalp (sort-by-frequency (list 0 1 2 3 4 5 6 7 8)   '< :key (function oddp)) '(1 3 5 7 0 2 4 6 8)))
(assert (equalp (sort-by-frequency (vector 2 3 5 3 7 9 5 3 7) '>) #(3 3 3 5 5 7 7 9 2)))



;; However, this solution doesn't match the expected result
;; "specified" above (notice also the implicit > order on the
;; frequencies).


;; [sourcecode lang="css"]

;; The example in the specification calls for a stable sort, therefore
;; we will have to keep the index of the first representant and use it
;; as secondary key in the sort operation.


(defun stable-sort-by-frequency (sequence lessp &key (key (function identity)))
  "
Sorts the elements in the SEQUENCE based on the frequency of their KEY
and the position of their first representant in the SEQUENCE.
LESSP shall be an order function on REAL.
"
  (flet ((compute-frequencies (sequence key)
           (let ((table (make-hash-table))
                 (index -1))
             (map nil (lambda (item)
                        (incf index)
                        (let ((item-key (funcal key item)))
                          (unless (gethash item-key table)
                            (setf (gethash item-key table) (list index)))
                          (push item (gethash item-key table))))
               sequence)
             table))
         (wrap (table)
           (let ((frequencies (make-array (hash-table-count table)))
                 (i -1))
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (let ((r (nreverse v)))
                          (setf (aref frequencies (incf i))
                                (cons (length (cdr r)) r))))
                      table)
             frequencies))
         (sort-frequencies (frequencies lessp)
           (sort frequencies
                 (lambda (a b)
                   (cond
                     ((funcall lessp (first a) (first b)) t)
                     ((funcall lessp (first b) (first a)) nil)
                     (t                                   (< (second a) (second b)))))))
         (unwrap (result frequencies)
           (if (listp result)
               (loop
                 :with current := result
                 :for (count nil . elements) :across frequencies
                 :do (replace current elements)
                     (setf current (nthcdr count current)))
               (loop
                 :with start := 0
                 :for (count nil . elements) :across frequencies
                 :do (replace result elements :start1 start)
                     (incf start count)))
           result))
    (unwrap sequence
            (sort-frequencies (wrap (compute-frequencies sequence key)) lessp))))

(assert (equalp (stable-sort-by-frequency (list 1 2 3 4 5 6 7)       '< :key (function oddp)) '(2 4 6 1 3 5 7)))
(assert (equalp (stable-sort-by-frequency (list 0 1 2 3 4 5 6 7 8)   '< :key (function oddp)) '(1 3 5 7 0 2 4 6 8)))
(assert (equalp (stable-sort-by-frequency (vector 2 3 5 3 7 9 5 3 7) '>) #(3 3 3 5 5 7 7 2 9)))


;; [/sourcecode]

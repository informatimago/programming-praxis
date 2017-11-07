;; https://programmingpraxis.com/2017/11/07/binary-search-with-duplicates/
;;
;; Binary Search With Duplicates
;; November 7, 2017
;;
;; Most implementations of binary search assume that the target array has
;; no duplicates. But sometimes there are duplicates, and in that case
;; you want to find the first occurrence of an item, not just any one of
;; several occurrences. For instance, in the array
;; [1,2,2,3,4,4,4,4,6,6,6,6,6,6,7] the first occurrence of 4 is at
;; element 4 (counting from 0), the first occurrence of 6 is at element
;; 8, and 5 does not appear in the array.
;;
;; Your task is to write a binary search that finds the first occurrance
;; of a set of duplicate items in a sorted array. When you are finished,
;; you are welcome to read or run a suggested solution, or to post your
;; own solution or discuss the exercise in the comments below.


;; [sourcecode lang="css"]

;; This problem is no different than the usual binary search (dichotomy),
;; but with a modified comparison  function.  Now, if the current element
;; is equal to the target value,  but the previous element is also equal,
;; then the current element must be considered greater.

(defun binary-search-first (vector value compare &key
                                                   (start 0) (end (length vector))
                                                   (key (function identity)))
  "
COMPARE: A comparing two elements A and B, and returning an order
         (signed integer), such as:
             A<B <=> result<0
             A=B <=> result=0
             A>B <=> result>0
START:   The minimum index.
END:     The maximum index+1.
RETURN:  (values found index order)
POST:	 (<= start index (1- end))
         +-------------------+----------+-------+----------+
         | Case              |  found   | index |  order   |
         +-------------------+----------+-------+----------+
         | x < a[i]          |   FALSE  | start |  less    |
         | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |
         | x = a[i]          |   TRUE   |   i   |  equal   |
         | a[max] < x        |   FALSE  | end-1 |  greater |
         +-------------------+----------+-------+----------+
"
  (com.informatimago.common-lisp.cesarum.utility:dichotomy
   (lambda (current)
     (if (= current start)
         (funcall compare value (funcall key (aref vector current)))

         (let ((order (funcall compare value (funcall key (aref vector current)))))
           (if (and (zerop order)
                    (zerop (funcall compare value (funcall key (aref vector (1- current))))))
               -1
               order))))
   start end))

(binary-search-first #(1 2 2 3 4 4 4 4 6 6 6 6 6 6 7) 4 (lambda (a b)
                                                          (cond ((< a b) -1)
                                                                ((> a b) +1)
                                                                (t        0))))
;; --> t
;;     4
;;     0

;; [/sourcecode]


;; https://programmingpraxis.com/2017/11/21/floor-and-ceiling-in-an-array/
;;
;; Floor And Ceiling In An Array
;; November 21, 2017
;;
;; We looked at variants of binary search in two recent exercises. Today
;; we look at a third variant.
;;
;; Your task is to write a variant of binary search in a sorted array
;; without duplicates that returns the index of the two elements
;; immediately below and above a target; if the target is in the array,
;; both return values should point to the target value. When you are
;; finished, you are welcome to read or run a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.

;; [sourcecode lang="css"]

;; This is when you're happy to have in your library a well defined
;; generic operation.  So you don't have to re-implement and re-debug
;; it each time, and so that this kind of user request changes are
;; trivial to implement.  Notice also that as usual, the customer make
;; ludicruous demands, such as having two indexes when it's obviously
;; not possible when the target outside of the vector.  We'll return a
;; NIL in place of the missing index.


(defun binary-search-floor-ceiling (vector value compare
                                    &key
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
RETURN:  (values index-floor index-ceiling)
"
  (multiple-value-bind (found index order)
      (com.informatimago.common-lisp.cesarum.utility:dichotomy-search
       vector value compare :start start :end end :key key)
    (cond
      (found               (values index index))
      ((minusp order)      (values nil start))
      ((= index (1- end))  (values (1- end) nil))
      (t                   (values index (1+ index))))))

(loop with vector = #(1 3 5 7 9)
      for target from 0 to 10
      collect (multiple-value-list (binary-search-floor-ceiling
                                    vector target (lambda (a b)
                                                    (cond ((< a b) -1)
                                                          ((> a b) +1)
                                                          (t        0))))))
;; --> ((nil 0) (0 0) (0 1) (1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (4 4) (4 nil))

;; [/sourcecode]

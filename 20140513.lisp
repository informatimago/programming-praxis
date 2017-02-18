
;; [sourcecode lang="css"]
;; 1) Consider a sorted singly-linked list having the following
;; nodes: 10 -> 30 -> 50 -> 70 -> NULL. You are given a pointer to
;; node 50 and a new node having the value 40. Can you insert node
;; 40 correctly in the list maintaining the ascending order?


(defun insert-at-node (node data)
  (setf (cdr node) (cons (car node) (cdr node))
        (car node) data)
  node)

(let ((list (list 10 30 50 70)))
  (insert-at-node (nthcdr 2 list) 40)
  list)
;; --> (10 30 40 50 70)
;; [/sourcecode]
;;
;; This is correct, as long as the list is is originally in the ascending
;; order, and the node given to insert-at-node is effectively the node
;; containing the smallest item greater than the new data.


;; [sourcecode lang="css"]
;; 2) Given a linked list 5 -> 4 -> 3 -> 2 -> 1 produce a linked
;; list 4 -> 2 -> 0 -> 2 -> 1 by subtracting the last node of the
;; list from the first, the next-to-last node from the second, and
;; so on, stopping at the midpoint of the list.

(defun palinsub (list)
  (loop
    :for slow = list :then (cdr slow)
    :for fast = list :then (cddr fast)
    :while (cdr fast)
    :finally (return (append (mapcar (function -) list (reverse slow))
                             (if fast
                                 (cdr slow)
                                 slow)))))

(assert (equalp (palinsub '(5 4 3 2 1)) '(4 2 0 2 1)))
(assert (equalp (palinsub '(5 4 3 2))   '(3 1 3 2)))
(assert (equalp (palinsub '(3))         '(0)))
(assert (equalp (palinsub '())          '()))
;; [/sourcecode]


;; [sourcecode lang="css"]
;; 3) Write a program to output the number of consecutive trailing
;; zeros in the factorial of a number. For example, if the number is
;; 5, then 5! = 120, and there is one trailing zero.

(defun count-factors-in-n (n f)
  (if (zerop n)
      0
      (loop
        :with quo
        :with rem
        :do (multiple-value-setq (quo rem) (truncate n f))
            (setq n quo)
        :while (zerop rem)
        :sum 1)))

(defun count-factors-in-factorial-n (n f)
  (loop
    :for i :from 1 :to n :sum (count-factors-in-n i f)))

(progn
  (assert (= (count-factors-in-factorial-n  4 5) 0))
  (assert (= (count-factors-in-factorial-n  5 5) 1))
  (assert (= (count-factors-in-factorial-n  6 5) 1))
  (assert (= (count-factors-in-factorial-n 24 5) 4))
  (assert (= (count-factors-in-factorial-n 25 5) 6))
  (assert (= (count-factors-in-factorial-n 27 5) 6)))
;; [/sourcecode]
;;
;; For any n>=2, in the decomposition in prime factors of n!, the
;; exponent of 2 is greater than the exponent of 5,
;; since (log n 2)/(log n 5) = (log 5 2) ~= 2.321928 > 1
;;
;; Since only the multiples of an exponent of 10 will add a 0 to the
;; factorial, and there are more multiples of 2 than multiples of 5, we
;; can just sum the exponents of 5 factor by factor.



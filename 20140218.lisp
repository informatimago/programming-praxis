;;; http://programmingpraxis.com/2014/02/18/two-interview-questions/

;; 1) Given a number n, find the smallest 3-digit number such that
;; the product of its digits is equal to n. For example, given n =
;; 100, the solution is 455.

(defun find-3-digit-product (n)
  (cond
    ((= 0 n) 0)
    ((<= 1 n (* 9 9 9))
     (loop
       :named :search
       :for a :from 0 :to 9
       :when (and (plusp a) (zerop (rem n a)))
         :do (loop
               :for b :from 0 :to 9
               :when (and (plusp (* a b)) (zerop (rem n (* a b))))
                 :do (loop
                       :for c :from 0 :to 9
                       :when (= (* a b c) n)
                         :do (return-from :search (+ (* 100 a) (* 10 b) c))))))))



;; 2) Given two arrays, one with n items and one with n+2 items
;; including the same items as the array with n items plus two
;; additional items, find the two additional items. Assume none of
;; the items are duplicates.

(defun find-2-supplemental/simple (v two-more &key (test (function eql)) (key (function identity)))
  (set-difference (coerce two-more 'list) (coerce v 'list) :test test :key key))

(find-2-supplemental/simple #(given two arrays)  #("given" "TWO" arrays "one" with)
                            :test (function string-equal))
--> (with "one")

;; It is expected that implementations use an O(n) algorithm for
;; set-difference when the test function is one of eql, equal or
;; equalp, namely, the elements are put in a hash-table.  But when
;; it's not possible it may degenerate to O(n²).


;; If we can assume the items are numbers without overflow (eg. lisp
;; integers or rationals), we can use a trick, solving the problem by
;; solving the equation system for the unknown a and b:
;; a+b = Σu-Σv
;; a*b = Πu/Πv
;; which gives a O(2n+2) = O(n) in time, and O(1) in space solution.


;; Δ=((a+b * a+b) - 4*a*b)
;; Δ=a²+2ab+b²-4ab = a²-2ab+b² = (a-b)² ⇒ √Δ = a-b
;; therefore we can use a simplifed quadratic equation solver:

(defun equa2 (a b c)
  (let ((delta (- (* b b) (* 4 a c))))
    (/ (+ (- b) (sqrt delta)) 2 a)))


(defun find-2-supplemental/efficient (v two-more)
  (let* ((a+b (- (reduce '+ two-more) (reduce '+ v)))
         (a*b (/ (reduce '* two-more) (reduce '* v)))
         (b   (equa2 1 (- a+b) a*b))
         (a   (- a+b b)))
    (list a b)))


find-2-supplemental/efficient #(1 2 3 4 5 6) #(0 1 2 3 4 5 6 7))
--> (0 7)
(find-2-supplemental/efficient #(1 2 3 4 5 6) #(42 1 2 3 4 5 6 66))
--> (42 66)


(defun find-2-supplemental/more-efficient (v two-more)
  (let ((a+b 0)
        (a*b 1))
    (loop
      :for n :across two-more
      :do (setf a+b (+ a+b n)
                a*b (* a*b n)))
    (loop
      :for n :across v
      :do (setf a+b (- a+b n)
                a*b (/ a*b n)))
    (let* ((b   (equa2 1 (- a+b) a*b))
           (a   (- a+b b)))
      (list a b))))

(find-2-supplemental/more-efficient #(1 2 3 4 5 6) #(0 1 2 3 4 5 6 7))
--> (0 7)
(find-2-supplemental/more-efficient #(1 2 3 4 5 6) #(42 1 2 3 4 5 6 66))
--> (42 66)

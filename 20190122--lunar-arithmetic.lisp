;; Lunar Arithmetic
;; 
;; January 22, 2019
;; 
;; Over at Numberphile, Neil Sloane (yes, that Neil Sloane), talks about
;; lunar arithmetic, an “other-worldly” way of doing simple math:
;; 
;;     Lunar addition and multiplication work digit by digit, the same as
;;     terrestrial addition and multiplication, except that the plus and
;;     times tables are unusual. Addition is done by taking the larger of
;;     the two digits, so 1 + 3 = 3 and 7 + 4 = 7. Multiplication is done
;;     by taking the smaller of the two digits, so 1 * 3 = 1 and 7 * 4 =
;;     4. Thus:
;;    
;;           3 5 7        3 5 7
;;         +   6 4      *   6 4
;;         -------      -------
;;           3 6 7        3 4 4
;;                      3 5 6
;;                      -------
;;                      3 5 6 4
;;     
;;     There are no carries. There is no subtraction or division, since
;;     the results would not be unique.
;;    
;; Your task is to write programs that perform lunar addition and
;; multiplication. When you are finished, you are welcome to read or run
;; a suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

;; [sourcecode lang="css"]

(defpackage "LUNAR"
  (:use)
  (:export "+" "*"))

(defpackage "LUNAR-ARITHMETIC"
  (:use "COMMON-LISP"))
(in-package "LUNAR-ARITHMETIC")

(deftype natural () `(integer 0))
(deftype digit   () `(integer 0 9))

(defun make-decimal (length &optional (default-digit 0))
  (make-array length :element-type 'digti :initial-element default-digit))

(defun decimal-from-integer (n)
  (check-type n natural)
  (loop
    :with result := (make-decimal (if (< n 2) 1 (ceiling (log n 10))))
    :while (plusp n)
    :for i :from 0
    :for (q d) := (multiple-value-list (truncate n 10))
    :do (setf (aref result i) d
              n q)
    :finally (return result)))

(defun decimal-to-integer (digits)
  (reduce (lambda (d n) (+ (* n 10) d))
          digits
          :initial-value 0
          :from-end t))

(defun lunar:+ (&rest arguments)
  (cond
    ((null arguments) 0)
    ((null (rest arguments)) (first arguments))
    (t
     (let* ((arguments (mapcar (function decimal-from-integer) arguments))
            (result    (make-decimal (reduce (function max) arguments :key (function length)))))
       (replace result (pop arguments))
       (dolist (argument arguments)
         (map-into result (function max) result argument))
       (decimal-to-integer result)))))

(defun binary* (a b)
  (let ((result (make-decimal (1- (+ (length a) (length b))))))
    (loop
      :for bd :across b
      :for start :from 0
      :do (loop
            :for ad :across a
            :for i :from start 
            :do (setf (aref result i) (max (aref result i) (min ad bd)))))
    result))

(defun lunar:* (&rest arguments)
  (if (null arguments)
      1
      (loop
        :with arguments := (mapcar (function decimal-from-integer) arguments)
        :while (rest arguments)
        :do (setf arguments (loop
                              :for (a b) :on arguments :by (function cddr)
                              :if b
                                :collect (binary* a b)
                              :else
                                :collect a))
        :finally (return (decimal-to-integer (first arguments))))))

(assert (equal (list (lunar:+ 357 64)
                     (lunar:* 357 64)
                     (lunar:+ 123 65400)
                     (lunar:+ 54321 9876004 4)
                     (lunar:* 54321 9000004 4)
                     (lunar:* 22 54321)
                     (lunar:* 54321 9 8 7 6 5 4)
                     (lunar:* 54321 44)
                     (lunar:+ 54321 9000004))
               '(367 3564 65423 9876324 44321044321 222221 44321 444321 9054324)))

;; [/sourcecode]



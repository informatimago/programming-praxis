;; https://programmingpraxis.com/2017/10/20/partial-products-of-an-array/
;;
;; Partial Products Of An Array
;; 
;; October 20, 2017
;; 
;; We have another homework problem today:
;; 
;; Replace each element of an array with the product of every other
;; element of the array, without using the division operator. For
;; instance, given array (5 3 4 2 6 8), the desired output is (1152 1920
;; 1440 2880 960 720).
;; 
;; Your task is to write a program to replace each element of an array
;; with the product of every other element of the array, without
;; performing division. When you are finished, you are welcome to read or
;; run a suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.


(defun partial-product (vector)
  ;; O(n^2) multiplications
  (loop
    :with len := (length vector)
    :with result := (make-array len)
    :for i :below len
    :do (setf (aref result i) (loop
                                :with p := 1
                                :for j :below len
                                :do (when (/= i j)
                                      (setf p (* p (aref vector j))))
                                :finally (return p)))
    :finally (return result)))

(partial-product #(5 3 4 2 6 8))
#(1152 1920 1440 2880 960 720)


;; a b c d e f
;; -----------
;; 1 b c d e f
;; a 1 c d e f
;; a b 1 d e f
;; a b c 1 e f
;; a b c d 1 f
;; a b c d e 1

(defun partial-product (vector)
  (let* ((len    (length vector))
         (result (make-array len :initial-element 1)))
    (labels ((product (min max)
               (if (< min max)
                   (loop
                     :with p := (aref vector min)
                     :for i :from (1+ min) :below max
                     :do (setf p (* p (aref vector i)))
                     :finally (return p))
                   1))
             (factor (min max p)
               (loop
                 :for j :from min :below max
                 :do (setf (aref result j) (* (aref result j) p))))
             (multiply (min max)
               (unless (= min max)
                 (if (evenp (- max min))
                     (let ((mid (+ min (truncate (- max min) 2))))
                       (factor mid max (product mid max))
                       (factor min mid (product min mid))
                       (multiply min mid)
                       (multiply mid max))
                     (let ((mid (+ min (truncate (- max min) 2))))
                       (factor min mid      (product (1+ mid) max))
                       (factor mid (1+ mid) (aref vector mid))
                       (factor (1+ mid) max (product min mid))
                       (multiply min mid)
                       (multiply (1+ mid) max))))))
      (multiply 0 len)
      result)))

(partial-product #(5 3 4 2 6 8))
#(1200 180 1200 1536 576 1536)
#(1152 1920 1440 2880 960 720)

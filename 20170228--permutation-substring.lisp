;; https://programmingpraxis.com/2017/02/28/a-fun-little-task/
;;
;; A Fun Little Task
;; by programmingpraxis
;; Today's exercise is a fun little task:
;; 
;; Given two strings a and b, with a no longer than b, determine if there
;; is any permutation of a that appears in b. For instance, a permutation
;; of string xyz appears starting at the fourth character (counting from
;; zero) of string afdgzyxksldfm.
;; 
;; Your task is to write a program to determine is any permutation of a
;; string is a substring of another string. When you are finished, you
;; are welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.

(defpackage "MULTISET"
  (:use "COMMON-LISP"))
(in-package "MULTISET")

(defgeneric elements (set)
  (:documentation "The elements in the set."))

(defclass multiset (#|set|#)
  ((data :initarg :data :accessor ms-data)))

(defun make-multiset (&key (test (function eql)))
  (make-instance 'multiset :data (make-hash-table :test test)))


(defmethod include               ((destination-set multiset) element)
  (incf (gethash element (ms-data destination-set) 0))
  destination-set)

(defmethod exclude               ((destination-set multiset) element)
  (if (= 1 (gethash element (ms-data destination-set) 0))
      (remhash  element (ms-data destination-set))
      (decf (gethash element (ms-data destination-set) 0)))
  destination-set)

(defmethod contains               ((set multiset) element)
  (plusp (gethash element (ms-data set) 0)))

(defmethod cardinal               ((set multiset))
  (let ((cardinal 0))
    (maphash (lambda (element count)
               (declare (ignore element))
               (incf cardinal count))
             (ms-data set))
    cardinal))

(defmethod elements               ((set multiset))
  (let ((elements '()))
    (maphash (lambda (element count)
               (loop :repeat count
                     :do (push element elements)))
             (ms-data set))
    elements))

(defmethod select               ((set multiset))
  (maphash (lambda (element count)
             (declare (ignore count))
             (return-from select element))
           (ms-data set))
  (values))

(defmethod map-elements           (result-type mapper (set multiset))
  (collecting-result (collect result-type)
    (maphash
     (lambda (element count)
       (loop :repeat count
             :do (collect (funcall mapper element))))
     (ms-data set))))

(defmethod make-collector        ((result-type (eql 'multiset)))
  (declare (ignorable result-type))
  (lambda (&optional set (element nil add-element-p))
    (if add-element-p
        (progn
          (include set element)
          set)
        (make-instance 'multiset))))

(defmethod minimum               ((set multiset))
  (let ((elements (elements set)))
    (when (every (function realp) elements)
      (reduce (function min) elements))))

(defmethod maximum               ((set multiset))
  (let ((elements (elements set)))
    (when (every (function realp) elements)
      (reduce (function max) elements))))


;; (let ((s (make-multiset)))
;;   (include s 1)
;;   (include s 1)
;;   (include s 1)
;;   (include s 2)
;;   (include s 2)
;;   (include s 3)
;;   (list (elements s)
;;         (minimum s)
;;         (maximum s)
;;         (select s)
;;         (cardinal s)
;;         (contains s 1)
;;         (contains s 0)))
;; ((2 2 1 1 1 3) 1 3 3 6 t nil)

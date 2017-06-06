;; https://programmingpraxis.com/2017/06/06/matrix-rotation/
;; 
;; We have a two-part exercise today, based on a Microsoft interview question.
;; 
;; First, write a program to rotate an m × n matrix 90° to the right, as
;; shown below; your solution should touch each matrix element only once:
;; 
;;     a b c
;;     d e f                 m j g d a
;; A = g h i        rot(A) = n k h e b
;;     j k l                 o l i f c
;;     m n o
;; 
;; Second, write a program to rotate a square matrix with n rows and
;; columns in-place. where the source and target matrices are the same
;; matrix and there is no intermediate matrix (be sure your solution
;; works for both even and odd n):
;; 
;;     a b c d e                 u p k f a
;;     f g h i j                 v q l g b
;; B = k l m n o        rot(B) = w r m h c
;;     p q r s t                 x s n i d
;;     u v w x y                 y t o j e
;; 
;; Your task is to write the two programs that rotate matrices. When you
;; are finished, you are welcome to read or run a suggested solution, or
;; to post your own solution or discuss the exercise in the comments
;; below.


(defun rotated-matrix (mat)
  (assert (= 2 (array-rank mat)))
  (let ((rot    (make-array (reverse (array-dimensions mat))))
        (srows  (array-dimension mat 0))
        (scols  (array-dimension mat 1)))
    (loop
      :for sr :below srows
      :for dr := (- srows sr 1)
      :do (loop
            :for c :below scols
            :do (setf (aref rot c dr) (aref mat sr c))))
    rot))

(assert (equalp (rotated-matrix #2A((a b c)
                                    (d e f)
                                    (g h i)
                                    (j k l)
                                    (m n o)))
                #2A((m j g d a)
                    (n k h e b)
                    (o l i f c))))

(assert (equalp (rotated-matrix #2A((a b c d e)
                                    (f g h i j)
                                    (k l m n o)
                                    (p q r s t)
                                    (u v w x y)))
                #2A((u p k f a)
                    (v q l g b)
                    (w r m h c)
                    (x s n i d)
                    (y t o j e))))



(defun rotate-square-matrix (mat)
  (assert (= 2 (array-rank mat)))
  (assert (= (array-dimension mat 0)
             (array-dimension mat 1)))
  (let ((size (array-dimension mat 0)))
    (loop
      :for r :below (truncate size 2)
      :do (loop
            :for c :from r :below (- size r 1)
            :do (rotatef (aref mat (- size c 1) r)
                         (aref mat (- size r 1) (- size c 1))
                         (aref mat c            (- size r 1))
                         (aref mat r c))))
    mat))

(let ((mat (make-array '(5 5) :initial-contents #(#(a b c d e)
                                                  #(f g h i j)
                                                  #(k l m n o)
                                                  #(p q r s t)
                                                  #(u v w x y)))))
  (rotate-square-matrix mat)
  (assert (equalp mat
                  #2A((u p k f a)
                      (v q l g b)
                      (w r m h c)
                      (x s n i d)
                      (y t o j e)))))

(let ((mat (make-array '(4 4) :initial-contents #(#(a b c d)
                                                  #(f g h i)
                                                  #(k l m n)
                                                  #(p q r s)))))
  (rotate-square-matrix mat)
  (assert (equalp mat
                  #2A((p k f a)
                      (q l g b)
                      (r m h c)
                      (s n i d)))))





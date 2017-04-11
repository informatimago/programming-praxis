;; https://programmingpraxis.com/2017/04/11/sort-four/
;;
;; Sort Four
;; 
;; Write a program that takes exactly four items as input and returns
;; them in sorted order.
;; 
;; Your task is to write a program that sorts exactly four items; can you
;; do it using only five comparisons? When you are finished, you are
;; welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.


(defun sort-vector (v lessp)
  (if (= 4 (length v))
      (sort-4 lessp (aref v 0) (aref v 1) (aref v 2) (aref v 3))
      (sort v lessp)))

(defun sort-4 (lessp a b c d)
  (if (funcall lessp a b)
      (if (funcall lessp c d)
          (if (funcall lessp a c)
              (if (funcall lessp b d)
                  (if (funcall lessp b c)
                      (vector a b c d) 
                      (vector a c b d))
                  (vector a c d b))
              (if (funcall lessp b d)
                  (vector c a b d)
                  (if (funcall lessp a d)
                      (vector c a d b) 
                      (vector c d a b))))
          (if (funcall lessp a d)
              (if (funcall lessp b c)
                  (if (funcall lessp b d)
                      (vector a b d c) 
                      (vector a d b c)) 
                  (vector a d c b)) 
              (if (funcall lessp b c)
                  (vector d a b c)
                  (if (funcall lessp a c)
                      (vector d a c b) 
                      (vector d c a b)))))
      (if (funcall lessp c d)
          (if (funcall lessp b c)
              (if (funcall lessp a d)
                  (if (funcall lessp a c)
                      (vector b a c d) 
                      (vector b c a d))
                  (vector b c d a)) 
              (if (funcall lessp a d)
                  (vector c b a d)
                  (if (funcall lessp b d)
                      (vector c b d a) 
                      (vector c d b a))))
          (if (funcall lessp b d)
              (if (funcall lessp a c)
                  (if (funcall lessp a d)
                      (vector b a d c) 
                      (vector b d a c)) 
                  (vector b d c a)) 
              (if (funcall lessp a c)
                  (vector d b a c)
                  (if (funcall lessp b c)
                      (vector d b c a) 
                      (vector d c b a)))))))

(assert (every (lambda (v)
                 (equalp (sort-4 (function <) (aref v 0) (aref v 1) (aref v 2) (aref v 3))
                         #(1 2 3 4)))
               '(#(1 2 3 4) 
                 #(2 1 3 4) 
                 #(2 3 1 4) 
                 #(2 3 4 1) 
                 #(1 3 2 4) 
                 #(3 1 2 4) 
                 #(3 2 1 4) 
                 #(3 2 4 1) 
                 #(1 3 4 2) 
                 #(3 1 4 2) 
                 #(3 4 1 2) 
                 #(3 4 2 1) 
                 #(1 2 4 3) 
                 #(2 1 4 3) 
                 #(2 4 1 3) 
                 #(2 4 3 1) 
                 #(1 4 2 3) 
                 #(4 1 2 3) 
                 #(4 2 1 3) 
                 #(4 2 3 1) 
                 #(1 4 3 2) 
                 #(4 1 3 2) 
                 #(4 3 1 2) 
                 #(4 3 2 1))))



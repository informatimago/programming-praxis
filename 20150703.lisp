;; I’m sorry I missed Tuesday’s exercise; I’ve been very busy at work.
;; Today’s exercise is an interview question of the kind I don’t like:
;; it’s tricky, you either know the answer or you don’t, and it’s
;; unlikely to be useful in any real programming situation: You are give
;; four integers x, y, a and b. Your first task is to assign a to y if x
;; = 0, or assign b to y if x = 1, without using any conditional
;; operator, including the ternary operator. Your second task is to
;; perform the same assignment without using any arithmetic operators.
;; Your task is to complete the two-part puzzle given above. When you
;; are finished, you are welcome to read or run a suggested solution, or
;; to post your own solution or discuss the exercise in the comments
;; below.

(defun trick (x a b)
  (values  (+ (* a (- 1 x)) (* b x))
           (logior (logand a (lognot (- x))) (logand b (- x)))
           (aref (vector a b) x)))


(trick 0 42 33)
42
42
42

(trick 1 42 33)
33
33
33

(trick 0 -42 -33)
-42
-42
-42

(trick 1 -42 -33)
-33
-33
-33

#|
uint64_t trick(uint64_t x,uint64_t a,uint64_t  b){
    x = (x <<  1) | x; x = (x <<  2) | x; x = (x <<  4) | x;
    x = (x <<  8) | x; x = (x << 16) | x; x = (x << 32) | x;
    return (b & x) | (a & (~x));}

|#

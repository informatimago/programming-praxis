;; During the development of Algol 60, Donald Knuth devised a nasty test
;; of recursion:
;;
;; There are quite a few ALGOL60 translators in existence which have been
;; designed to handle recursion and non-local references properly, and I
;; thought perhaps a little test-program may be of value. Hence I have
;; written the following simple routine, which may separate the
;; man-compilers from the boy-compilers:
;;
;;     begin
;;      real procedure A(k, x1, x2, x3, x4, x5);
;;      value k; integer k;
;;      begin
;;       real procedure B;
;;       begin
;;        k := k - 1;
;;        B := A := A(k, B, x1, x2, x3, x4)
;;       end;
;;       if k ≤ then A : = x4 + x5 else B
;;      end
;;      outreal(A(10, 1, -1, -1, 1, 0))
;;     end
;;
;; This uses nothing known to be tricky or ambiguous. My question is:
;; What should the answer be? Unfortunately, I don’t have to a
;; man-compiler myself, and so I was forced to try hand calculations. My
;; conjecture (probably wrong) is that the answer will be:
;;
;;     73 - 119 - 177 + 102 = - 121
;;
;; I’d be very glad to know the right answer.
;;
;; Your task is to write a program that computes the right answer. When
;; you are finished, you are welcome to read or run a suggested solution,
;; or to post your own solution or discuss the exercise in the comments
;; below.

#|

It is not a test of recursion, but a test of call-by-name.

You have to be careful to interpret the name of the functions in
Algol: depending on whether they are "left-values" or "right-values",
they will denote an assignment to a variable that will be the function
result, or they denote a (possibly recursive) function call.

Thus:

       B := A := A(k, B, x1, x2, x3, x4)
       ^    ^    ^    ^
       |    |    |    |
       |    |    |    +--- function call.
       |    |    +-------- function call.
       |    +------------- assignment to result variable
       +------------------ assignment to result variable


       if k ≤ then A : = x4 + x5 else B
                   ^                  ^
                   |                  |
                   |                  +---- function call
                   +----------------------- assignment to result variable

All the arguments to A, but the first one which is declared to be
by-value, are passed by-name.  This requires the creation of a thunk
(macro byname), and the evaluation of that thunk when the value is
eventually needed (function deref).

|#


(defun deref (x)
  (if (functionp x)
      (deref (funcall x))
      x))

(defmacro byname (x)
  `(lambda () ,x))

(labels ((A (k x1 x2 x3 x4 x5)
           (let ((a :uninitialized-a))
             (labels ((b ()
                        (let ((b :uninitialized-b))
                          (decf k)
                          (setf b (setf a (a k (byname (b)) (byname x1) (byname x2) (byname x3) (byname x4))))
                          b)))
               (if (<= k 0)
                   (setf a (+ (deref x4) (deref x5)))
                   (b)))
             a)))
  (a 10 1 -1 -1 1 0))

;; --> -67


;; Longest Sequence Of Consecutive Odd Integers
;; October 20, 2015
;; 
;; Today’s exercise is to find the longest sequence of consecutive odd
;; integers that add to a given sum. For instance, given the target
;; 160701, the three consecutive odd integers 53565, 53567 and 53569 sum
;; to 160701, but the 391 consecutive odd integers from 21 to 801 also
;; sum to 160701, and are the longest sequence of consecutive odd
;; integers to do so, so they are the correct answer.
;; 
;; Your task is to write a program to find the longest sequence of
;; consecutive odd integers that add to a given sum. When you are
;; finished, you are welcome to read or run a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.



;; N = a+0 + a+2 + … a+(2k)
;; 
;;                k
;; N = a(k+1) + 2 Σi
;;               i=0
;; 
;;                 k(k+1)
;; N = a(k+1) + 2 --------
;;                    2
;; 
;; N = a(k+1) + k(k+1)
;; 
;; N = (a+k)(k+1)
;; 
;; Therefore we will find the length of the longuest sequence of
;; consecutive odd cardinals (starting from a) amongst the divisors of N
;; (minus one).
;;
;; a+k = N/(k+1)
;; 
;; a = N/(k+1)-k
;;
;; We'll have to find the odd cardinal a for the biggest k possible
;; amongst the divisors (k+1) of N.

(use-package  "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES")

(compute-primes-to  (isqrt 160701))
#(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397)


(defun longuest-sequence-of-consecutive-odd-cardinals (n)
  (check-type n (integer 3))
  (loop :for divisor :in (divisors n))
  (loop :for k+1 :in (rest (nreverse (rest (divisors n))))
        :for a := (- (/ n k+1) (- k+1 1))
        :do (print (list a k+1))
        :when (and (plusp a) (oddp a))
          :do (return (values a k+1))))

(longuest-sequence-of-consecutive-odd-cardinals 160701)
(divisors 160701)
(1 3 17 51 23 69 391 1173 137 411 2329 6987 3151 9453 53567 160701)

(-53563 53567) 
(-9435 9453) 
(-3099 3151) 
(-6963 6987) 
(-2259 2329) 
(-19 411) 
(1037 137) 1037
137

(-53563 53567) 
(-9435 9453) 
(-3099 3151) 
(-6963 6987) 
(-2259 2329) 
(-19 411) 
(1037 137)
1037
137

(53565 3) 53565
3

(160701 1) 160701
1
160701
1

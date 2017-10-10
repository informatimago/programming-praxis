;; Day 280
;; by programmingpraxis
;; 
;; Pat Ballew is a retired math teacher who writes a blog On This Day In
;; Math that gives a day-by-day history of mathematics. The blog is odd,
;; quirky, and unquestionably fun. On October 7th, Ballew wrote:
;; 
;; The 280th day of the year.... The sum of the first 280 consecutive
;; primes, mod 280, is prime.
;; 
;; Since I like to play with prime numbers, that got my attention, and I
;; quickly went to determine how many such days there are in a year.
;; 
;; Your task is to determine how many days in a year share the
;; characteristic that on the nth day the sum of the first n primes, mod
;; n, is prime. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.


;; [sourcecode lang="css"]

(defun primep (n)
  (let ((factorization (com.informatimago.common-lisp.arithmetic.primes:factorize n)))
    (and (= 2 (length factorization))
         (integerp (second factorization)))))

(defparameter *primes*
  (cons 10000
        (com.informatimago.common-lisp.arithmetic.primes:compute-primes-to 10000)))

(defun sum-primes-mod (n)
  "Sums the first n primes, modulo n"
  (loop
    :while (< (length (cdr *primes*)) n)
    :do (setf *primes*
              (let ((m (truncate (* (car *primes*) (car *primes*)))))
                (cons m
                      (com.informatimago.common-lisp.arithmetic.primes:compute-primes-to
                       m)))))
  (mod (loop :repeat n
             :for p :across (cdr *primes*)
             :sum p)
       n))

(length (loop :for d :from 1 :to 366
              :when (primep (sum-primes-mod d))
                :collect d))
;; --> 108

(loop :for d :from 1 :to 366
              :when (primep (sum-primes-mod d))
                :collect d)
;; --> (5 6 7 8 12 15 16 19 20 21 24 26 30 34 37 38 40 42 44 45 46 48 49
;;     50 55 58 59 60 62 64 65 66 67 68 70 72 73 75 76 78 86 87 88 92 102
;;     116 120 122 124 128 130 132 135 140 143 145 150 156 158 164 165
;;     166 168 172 173 175 176 182 183 191 196 210 214 216 218 223 234
;;     236 241 248 250 256 259 262 265 266 272 280 285 301 306 310 311
;;     314 315 324 328 330 336 337 344 347 348 349 352 355 358 365)

;; [/sourcecode]


#|

A recent article in the Wall Street Journal discusses the "hot hand"
paradox. Basketball players especially believe in the hot hand, when
making a shot you can suddenly have a hot hand and make several more
shots in a row. The Wall Street Journal proposes this experiment:

    Toss a coin four times. Write down the percentage of heads on the
    flips coming immediately after heads. Repeat that process one
    million times. On average, what is that percentage?

The article claims that the correct percentage is 40%, not the 50%
that you would expect from an unbiased coin, and that this is proof
that the hot hand exists, even from such a random source as coin
flips. If you're interested, you can read the academic article that
the Wall Street Journal account is based on, or see discussion of it
at Hacker News.

Your task is to write a program to confirm the 40% figure. When you
are finished, you are welcome to read or run a suggested solution, or
to post your own solution or discuss the exercise in the comments
below.


|#

(defun hand ()     (list (random 2) (random 2) (random 2) (random 2)))
(defun hotp (hand) (mapcon (lambda (hand)
                             (when (rest hand)
                               (list (if (< (first hand) (second hand))
                                                    1
                                                    0))))
                           hand))
(defun hot% (hand) (/ (reduce (function +)
                              (mapcon (lambda (hand)
                                        (when (rest hand)
                                          (list (if (< (first hand) (second hand))
                                                    1
                                                    0))))
                                      hand))
                      (1- (length hand))))
(defun average-hot-hands (n)
  (/ (loop :repeat n :sum (hot% (hand))) n))

(loop :repeat 10 :collect (float (average-hot-hands 1000000)))
;; --> (0.24987033 0.25014567 0.25011367 0.25006 0.24962667 0.250003 0.24993366 0.25007135 0.249844 0.24986434)

(loop :repeat 10 :collect (loop :repeat 4000000 :sum (random 2)))
;; --> (2000662 2000325 1999560 2000706 1999913 2000742 1999843 1998853 1999672 1999963)

(loop :repeat 10 :collect (hotp (hand)))
;; --> ((0 0 1) (0 1 0) (0 0 0) (0 1 0) (0 0 1) (0 0 0) (0 0 1) (0 0 0) (0 0 0) (0 0 1))

Note: (1 1 1) is not possible, since after a head/flip subsequence,
you cannot have another head/flip, only either flip/flip or flip/head.

Therefore the only numbers of head/flip you can have are 0, 1 or 2
head/flip/head/flip
(hotp '(0 1 0 1))

Therefore we're averaging n random choices in {0/3 1/3 2/3}
((0/3)(n/3)+(1/3)(n/3)+(3/3)(n/3))/n
= (0 + n/9 + n/3)/n
= 1/9+1/3 = 1/9 + 3/9 = 4/9

(loop repeat 1000000 when (equal '(1 0 1) (hotp (hand))) count 1)
62367



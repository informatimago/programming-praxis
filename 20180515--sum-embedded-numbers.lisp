;; https://programmingpraxis.com/2018/05/15/sum-embedded-numbers/
;;
;; Sum Embedded Numbers
;; 
;; May 15, 2018
;; 
;; It’s mid-May, so at most universities the semester has ended and it is
;; safe for me to solve some of the exercises that students sent to me
;; over the last few months. Here’s a fun little one:
;; 
;; 
;; 
;; Given a string containing embedded numbers, find the sum of the
;; embedded numbers. For instance, given the string “11aa22bb33cc44”, the
;; desired sum is 11 + 22 + 33 + 44 = 110. You may not use regular
;; expressions.
;; 
;; 
;; 
;; Although the problem statement doesn’t say so, you may assume that the
;; numbers of interest are non-negative integers. Thus, the purpose of
;; the exercise is for students to iterate through a string, identify the
;; digits in the string, and manipulate them numerically.
;; 
;; 
;; 
;; Your task is to write a program that sums the numbers embedded in a
;; string. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

;; [sourcecode lang="css"]
(defun sum-embedded-cardinals (string)
  (loop
    :for start := (position-if (function digit-char-p) string)
      :then (position-if (function digit-char-p) string :start end)
    :for (cardinal end) := (when start
                             (multiple-value-list
                              (parse-integer string :start start :junk-allowed t)))
    :while cardinal
    :sum cardinal))

(defun test/sum-embedded-cardinals ()
  (assert (= 0   (sum-embedded-cardinals "")))
  (assert (= 0   (sum-embedded-cardinals "foo")))
  (assert (= 42  (sum-embedded-cardinals "42")))
  (assert (= 42  (sum-embedded-cardinals "-42")))
  (assert (= 42  (sum-embedded-cardinals "-42-")))
  (assert (= 10  (sum-embedded-cardinals "1-2-3-4")))
  (assert (= 10  (sum-embedded-cardinals "    1     2,    3;    4.  ")))
  (assert (= 110 (sum-embedded-cardinals "11aa22bb33cc44")))
  :success)


(test/sum-embedded-cardinals)
;; --> :success
;; [/sourcecode]


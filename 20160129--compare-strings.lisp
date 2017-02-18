(in-package :cl-user)

;; Compare Strings With One Error
;; by programmingpraxis
;;
;; I don't know if today's exercise comes from an interview question or
;; from somebody's homework, but it's a good exercise:
;;
;;     Given two strings, determine if they are the same except for
;;     exactly one difference. Two identical strings fail to match. Two
;;     strings that differ in two or more characters fail to match. Two
;;     strings with lengths that differ by one match if and only if they
;;     are identical except for one additional character. Indicate the
;;     index where the difference occurs, or report failure if the two
;;     input strings do not differ in exactly one character.
;;
;; Your task is to write a program that compares strings with one
;; error. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

#|
In Common Lisp, we can use mismatch to find where two sequences differ.
However, we have several cases to consider:

different lengths:

    "abc"
    "ab"

    "abc"
     "bc"

    "abcde"
    "abde"

same length:

    "abcde"
    "abxde"

|#

(defun string-equal-with-one-error (a b)
  ;; time complexity: O(length(a))
  ;; space complexity: O(1)
  (cond
    ((= (length a) (length b))
     (let ((difference (mismatch a b)))
       (and difference
            (string= a b :start1 (1+ difference)
                         :start2 (1+ difference))
            difference)))
    ((= (1+ (length a)) (length b))
     (string-equal-with-one-error b a))
    ((= (length a) (1+ (length b)))
     ;; a is longuest
     (if (string= a b :start1 1)
         0
         (let ((difference (mismatch a b)))
           (and difference
                (string= a b :start1 (1+ difference)
                             :start2 difference)
                difference))))
    (t
     nil)))

(defun test/string-equal-with-one-error ()
  (assert (null (string-equal-with-one-error "" "")))
  (assert (null (string-equal-with-one-error "a" "a")))
  (assert (null (string-equal-with-one-error "abc" "abc")))
  (assert (null (string-equal-with-one-error "" "abc")))
  (assert (null (string-equal-with-one-error "abc" "")))
  (assert (null (string-equal-with-one-error "abc" "ade")))
  (assert (null (string-equal-with-one-error "abc" "dbe")))
  (assert (= 0 (string-equal-with-one-error "abc" "dbc")))
  (assert (= 1 (string-equal-with-one-error "abc" "adc")))
  (assert (= 2 (string-equal-with-one-error "abc" "abd")))
  (assert (= 0 (string-equal-with-one-error "" "a")))
  (assert (= 0 (string-equal-with-one-error "a" "")))
  (assert (= 0 (string-equal-with-one-error "bcd" "abcd")))
  (assert (= 0 (string-equal-with-one-error "abcd" "bcd")))
  (assert (= 3 (string-equal-with-one-error "abc" "abcd")))
  (assert (= 3 (string-equal-with-one-error "abcd" "abc")))
  (assert (= 2 (string-equal-with-one-error "abcd" "abed")))
  (assert (= 2 (string-equal-with-one-error "abed" "abcd")))
  :success)

(test/string-equal-with-one-error)
;; --> :success



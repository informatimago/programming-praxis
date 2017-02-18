
;; Reverse String Ignoring Special Characters
;; October 30, 2015
;;
;; Today’s exercise solves a homework problem for some lucky reader:
;;
;;     Given an input string that contains both alphabetic characters and
;;     non-alphabetic characters, reverse the alphabetic characters while
;;     leaving the non-alphabetic characters in place. For instance,
;;     given the input string a!b3c, return the output string c!b3a,
;;     where the non-alphabetic characters ! and 3 are in their original
;;     positions.
;;
;; Your task is to write a program that reverses a string while leaving
;; non-alphabetic characters in place. When you are finished, you are
;; welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.

(in-package :cl-user)

(defgeneric nreverse-elements-such (predicate sequence)
  (:method (predicate (sequence list))
    (replace sequence (nreverse-elements-such predicate (coerce sequence 'vector))))
  (:method (predicate (sequence vector))
    (loop
      :with i := 0
      :with j := (1- (length sequence))
      :while (< i j)
      :do (cond
            ((not (funcall predicate (aref sequence i)))
             (incf i))
            ((not (funcall predicate (aref sequence j)))
             (decf j))
            (t (rotatef (aref sequence i)  (aref sequence j))
               (incf i)
               (decf j))))
    sequence))

(defgeneric reverse-elements-such (predicate sequence)
  (:method (predicate (sequence list))
    (coerce (nreverse-elements-such predicate (coerce sequence 'vector))
            'list))
  (:method (predicate (sequence vector))
    (nreverse-elements-such predicate (copy-seq sequence))))


(defun test/reverse-elements-such ()
  (let ((tests '(((1 2 3 4 5 6 7 8) oddp (7 2 5 4 3 6 1 8))
                 ((2 4 6 8 1 3 5 7) oddp (2 4 6 8 7 5 3 1))
                 ((1 3 5 7 2 4 6 8) oddp (7 5 3 1 2 4 6 8))
                 ((1 2 3 4 5 6 7 8) oddp (7 2 5 4 3 6 1 8))
                 ((1 3 5 2 4 6 7 8) oddp (7 5 3 2 4 6 1 8))
                 ((2 4 6 8 1 3 5)   oddp (2 4 6 8 5 3 1))
                 ((1 3 5 2 4 6 8)   oddp (5 3 1 2 4 6 8))
                 ((1 3 5 2 4 6 8)   oddp (5 3 1 2 4 6 8))
                 ((2 4 6 8)         oddp (2 4 6 8))
                 ((1 3 5 7)         oddp (7 5 3 1))
                 ((1)               oddp (1))
                 ((2)               oddp (2))
                 (()                oddp ()))))
    (loop :for (sequence predicate expected) :in tests
          :do (let ((input (copy-seq sequence)))
                (let ((output (nreverse-elements-such predicate input)))
                  (assert (eql input output))
                  (assert (equalp output expected))))
              (let ((input (coerce sequence 'vector)))
                (let ((output (nreverse-elements-such predicate input)))
                  (assert (eql input output))
                  (assert (equalp output (coerce expected 'vector)))))
              (let ((input (copy-seq sequence)))
                (let ((output (reverse-elements-such predicate input)))
                  (assert (or (null output) (not (eql input output))))
                  (assert (equalp input sequence))
                  (assert (equalp output expected))))
              (let ((input (coerce sequence 'vector)))
                (let ((output (reverse-elements-such predicate input)))
                  (assert (not (eql input output)))
                  (assert (equalp input (coerce sequence 'vector)))
                  (assert (equalp output (coerce expected 'vector)))))))
  :success)

(test/reverse-elements-such)
;; --> :success

(defun reverse-string-leaving-non-alphabetic-in-place (string)
  (reverse-elements-such (function alpha-char-p) string))

(defun test/reverse-string-leaving-non-alphabetic-in-place ()
  (assert (string= (reverse-string-leaving-non-alphabetic-in-place
                    "reverse-string-leaving-non-alphabetic-in-place")
                   "ecalpni-citeba-hplanon-gni-vaelgnirts-es-rever"))
  (assert (string= (reverse-string-leaving-non-alphabetic-in-place "a!b3c")
                   "c!b3a"))
  :success)

(test/reverse-string-leaving-non-alphabetic-in-place)
;; --> :success

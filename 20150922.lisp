;; From: programmingpraxis <post@gwene.org>
;; Subject: Three More Homework Problems
;; Newsgroups: gwene.com.yahoo.pipes.lisp
;; Date: Tue, 22 Sep 2015 11:00:40 +0200 (8 hours, 5 minutes, 29 seconds ago)
;; Message-ID: <x1-le3RLcHcayW9NBYPphQS/XP1z/w@gwene.org>
;;
;; I can tell from my statistics that people like the homework problem
;; exercises, I suppose because they are short and provide simple drill.
;; I can also tell, from reading some of the beginning-programmer
;; message boards, that students seem to be quicker that they used to be
;; to post their homework problems on-line rather than figuring out the
;; solutions themselves, either because they are lazy or because their
;; teachers aren’t providing sufficient help. So today we have three
;; more homework problems:
;;
;; Your task is to write programs that solve the three homework
;; problems. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

;; [sourcecode lang=’css’]
;; 1) Given a list of integers, return a list in which all the duplicates
;;    have been removed. For instance, given the input list [3, 2, 4, 2,
;;    7, 3, 5, 1, 3] return the output list [3, 2, 4, 7, 5, 1].
;; [/sourcecode]
;;
;; This exercise is a good example of what is wrong with specifications,
;; when you have one.  The specification is to return a list with the
;; duplicates removed, but it doesn't specify in what order the remaining
;; elements shall be placed in the resulting list, and which duplicates
;; shall be removed (the first occurence? the last? any?).  Such a weak
;; specification can be a good thing, because it allows the programmer to
;; implement usually very simple and efficient programs.  For example, we
;; could just write in Common Lisp:
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/badass1 (list-of-integers)
   (remove-duplicates list-of-integers ))
;; [/sourcecode]
;;
;; Unfortunately, the example given along with the specification, are
;; often to be understood as part of the specification, and as such, over
;; specify greatly.  In this case, the example implies an order of the
;; elements in the result list (same as the original list), and the
;; duplicates that must be deleted (the last ones).  Are those
;; constraints really needed?  And in TDD, those "examples" will often be
;; used in tests, thus, hardwiring possibly an overspecification.
;;
;; Happily, Common Lisp expected those requirements, and there's an
;; option to the REMOVE-DUPLICATES function to process the duplicates
;; from the end and therefore returning what the example expects:
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/badass (list-of-integers)
   (remove-duplicates list-of-integers :from-end t))
;; [/sourcecode]
;;
;; Of course, as a student exercise, such a solution might be rejected
;; (again, the problem statement lacks in formality, and probably relies
;; on a lot of assumptions).  So as indicated, you might want to write a
;; simple recursive solution such as:
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/On^2/out-of-specs (list-of-integers)
  (cond ((null list-of-integers)
         '())
        ((member (first list-of-integers) (rest list-of-integers))
         (remove-duplicate-integers/On^2 (rest list-of-integers)))
        (t (cons (first list-of-integers)
                 (remove-duplicate-integers/On^2 (rest list-of-integers))))))
;; [/sourcecode]
;;
;; Unfortunately this simple solution fails on three counts:
;; - it keeps the last duplicate instead of the first one,
;; - using member on the rest of the list, it is O(n²) in time;
;; - using a non-tail recursive call, it uses O(n) stack space.
;;
;; To transform the recursivity into a tail recursion, we can instead use
;; an accumulator, an additionnal parameter that will collect the
;; result. When using an accumulator to process a list, we obtain the
;; resulting list in the reverse order, and since the specification seems
;; to be unfortunately that the result must be in the same order as the
;; original list, we have to reverse the result.  Happily, this is only
;; O(n), so it won't be catastrophic.  On the other hand, we still use
;; member on the accumulated unique values, so this is still O(n²) in
;; time, even if the constants are less than previously, since we search
;; only the list of unique elements.
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/On^2 (list-of-integers)
  (labels ((remdup (uniques list)
             (cond ((null list)
                    (nreverse uniques))
                   ((member (first list) uniques)
                    (remdup uniques (rest list)))
                   (t
                    (remdup (cons (first list) uniques) (rest list))))))
    (remdup '()  list-of-integers)))
;; [/sourcecode]
;;
;; Finally, here is a solution that is O(n), uses O(1) stack space, and
;; O(n) temporary space, and returns the exact results specified by the
;; example: we use a hash-table to remember the unique elements.
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/On (list-of-integers)
  (let ((uniques (make-hash-table)))
    (dolist (element list-of-integers)
      (setf (gethash element uniques) 1))
    (mapcan (lambda (element)
              (unless (minusp (decf (gethash element uniques)))
                (list element)))
            list-of-integers)))

(mapc (lambda (rem) (assert (equal (funcall rem  '(3 2 4 2 7 3 5 1 3))
                                   '(3 2 4 7 5 1))))
      '(remove-duplicate-integers/badass
        remove-duplicate-integers/On^2
        remove-duplicate-integers/On))
;; [/sourcecode]
;;
;;
;;
;;
;; A O(n) remove duplicates that wouldn't have to keep the order of the
;; original list or select a precise duplicate to keep would be:
;;
;; [sourcecode lang=’css’]
(defun remove-duplicate-integers/On/relaxed (list-of-integers)
  (let ((uniques (make-hash-table)))
    (dolist (element list-of-integers)
      (setf (gethash element uniques) 1))
    (let ((results '()))
      (maphash (lambda (element present)
                 (declare (ignore present))
                 (push element results))
               uniques)
      results)))

(remove-duplicate-integers/On/relaxed '(3 2 4 2 7 3 5 1 3))
;; --> (7 2 1 3 5 4)
;; [/sourcecode]



;; [sourcecode lang=’css’]
;; 2) Given two lists of integers, each sorted in ascending order, return
;;    a list of the integers common to the two lists; the output list
;;    must also be in ascending order. For instance, give the input lists
;;    [2, 3, 5, 5, 6, 7, 8, 9] and [1, 2, 4, 5, 5, 7] return the output
;;    list [2, 5, 5, 7].

(defun common-integers (lista listb)
  (loop
    :for a := (first lista)
    :for b := (first listb)
    :while (and lista listb)
    :when (< a b)
      :do (pop lista)
    :when (> a b)
      :do (pop listb)
    :when (= a b)
      :collect (progn (pop lista) (pop listb))))

(common-integers '(2 3 5 5 6 7 8 9)  '(1 2 4 5 5 7))
;; --> (2 5 5 7)
;; [/sourcecode]




;; [sourcecode lang=’css’]
;; 3) Given a positive integer, determine if it is a perfect cube. For
;;    instance, the integer 125 is a perfect cube, 5*5*5, but the integer
;;    121 is not.

(defun perfect-cube-p (n)
  (assert (plusp n))
  (= n (expt (truncate (expt n 1/3)) 3)))

(perfect-cube-p 125) ; --> t
(perfect-cube-p 121) ; --> nil
;; [/sourcecode]



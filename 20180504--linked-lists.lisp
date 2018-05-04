;; https://programmingpraxis.com/2018/05/04/linked-list-exercises/
;;
;; Linked List Exercises
;; 
;; May 4, 2018
;; 
;; I like to do exercises on linked lists. In languages like C, linked
;; lists provide good drill for students who are uncertain about
;; structures and pointers; in any language, lists provide good drill on
;; recursion. Today’s exercise is about linked lists; we’ve seen some of
;; these before, but it’s good to review:
;; 
;; 
;; 
;; Take a list of integers and rearrange it so all the even integers
;; appear before all the odd integers, with both evens and odds appearing
;; in the output in the same order as the input.
;; 
;; Take a list of integers, split it into two lists each containing
;; alternate elements from the input list, then join the two lists back
;; together.
;; 
;; Take a list of integers and rearrange it so alternate nodes are each
;; greater than their two adjacent nodes; in other words, the integers
;; are in alternating high-low order.
;; 
;; Your task is to perform the three linked list exercises described
;; above. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.



;; [sourcecode lang= "css"]

;; Obviously, I'm in those exercises to show off the advantages of
;; Common Lisp.  The downside is that it usually defeats the purpose
;; of the exercise :-)  Those are typical.

(defun even-before-odd (loi)
  "Return  a list of the integers in LOI, rearranged it so all the
even integers appear before all the odd integers, with both evens and
odds appearing in the output in the same order as the input." 
  (stable-sort (copy-list loi) (lambda (a b) (when (evenp a) (oddp b)))))

(defun test/even-before-odd ()
  (assert (equal (even-before-odd '()) '()))
  (assert (equal (even-before-odd '(1)) '(1)))
  (assert (equal (even-before-odd '(1 3 1 3 5 7 5 7 1 3 1 3))
                 '(1 3 1 3 5 7 5 7 1 3 1 3)))
  (assert (equal (even-before-odd '(2 4 2 4 6 8 6 8 2 4 2 4))
                 '(2 4 2 4 6 8 6 8 2 4 2 4)))
  (assert (equal (even-before-odd '(1 3 1 3 5 7 2 4 2 4 6 8))
                 '(2 4 2 4 6 8 1 3 1 3 5 7)))
  (assert (equal (even-before-odd '(1 2 3 4 5 6 7 8 9))
                 '(2 4 6 8 1 3 5 7 9)))
  :success)


(defun reorder-alternating-elements (list)
 "Take a list of integers, split it into two lists each containing
alternate elements from the input list, then join the two lists back
together."

  (loop
    :with left := nil
    :for element :in list
    :if (setf left (not left))
      :collect element :into left-part
    :else
      :collect element :into right-part
    :finally (return (nconc left-part right-part))))

(defun test/reorder-alternating-elements ()
  (assert (equal (reorder-alternating-elements '())
                 '()))
  (assert (equal (reorder-alternating-elements '(a))
                 '(a)))
  (assert (equal (reorder-alternating-elements '(a 1))
                 '(a 1)))
  (assert (equal (reorder-alternating-elements '(a 1 b 2 c 3 d 4))
                 '(a b c d 1 2 3 4)))
  :success)


(defun zip (a b)
  (if a
      (if b
          (list* (car a) (car b) (zip (cdr a) (cdr b)))
          a)
      b))

(defun test/zip ()
  (assert (equal (zip '() '()) '()))
  (assert (equal (zip '(a) '()) '(a)))
  (assert (equal (zip '() '(1)) '(1)))
  (assert (equal (zip '(a b) '()) '(a b)))
  (assert (equal (zip '() '(1 2)) '(1 2)))
  (assert (equal (zip '(a) '(1)) '(a 1)))
  (assert (equal (zip '(a b c) '(1 2 3)) '(a 1 b 2 c 3)))
  (assert (equal (zip '(a b c) '(1 2 3 4 5)) '(a 1 b 2 c 3 4 5)))
  (assert (equal (zip '(a b c d e) '(1 2 3)) '(a 1 b 2 c 3 d e)))
  :success)

(defun alternate-high-low (lor)
  "Return a list of reals rearranged so alternate elements are each
greater than their two adjacent elements; in other words, the reals
are in alternating high-low order."
  ;; This implementation is O(nlogn) but walks the elements 6 more times!
  ;; But it has two advantages:
  ;; 1- it's correct,
  ;; 2- it's implemented quickly,
  ;; so you can start using it, and have time to develop and debug a more
  ;; optimized implementation (if needed).
  (let* ((sorted (sort (copy-list lor) (function <)))
         (length (length sorted)))
    (zip (subseq sorted 0 (ceiling length 2))
         (reverse (subseq sorted (ceiling length 2))))))

(defun test/alternate-high-low ()
  (flet ((alternatep (list)
           (or (null (cddr list))
               (loop
                 :while (cddr list)
                 :always (and (<= (first list) (second list))
                              (>= (second list) (third list)))
                 :do (setf list (cddr list)))
               (loop
                 :while (cddr list)
                 :always (and (>= (first list) (second list))
                              (<= (second list) (third list)))
                 :do (setf list (cddr list))))))
    (assert (alternatep (alternate-high-low '())))
    (assert (alternatep (alternate-high-low '(1))))
    (assert (alternatep (alternate-high-low '(1 2))))
    (assert (alternatep (alternate-high-low '(1 2 3))))
    (assert (alternatep (alternate-high-low '(1 2 3 4 5 6 7 8 9)))))
  :success)




;; slow                  fast               reversed   result
;; (1 2 3 4 5 6 7 8)     (1 2 3 4 5 6 7 8)
;;   (2 3 4 5 6 7 8)         (3 4 5 6 7 8)  (1)
;;     (3 4 5 6 7 8)             (5 6 7 8)  (2 1)
;;       (4 5 6 7 8)                 (7 8)  (3 2 1)
;;         (5 6 7 8)                    ()  (4 3 2 1)
;;           (6 7 8)                    ()  (3 2 1)    (4 5)
;;             (7 8)                    ()  (2 1)      (3 6 4 5)
;;               (8)                    ()  (1)        (2 7 3 6 4 5)
;;                ()                    ()  ()         (1 8 2 7 3 6 4 5)

;; slow                    fast                 reversed   result
;; (1 2 3 4 5 6 7 8 9)     (1 2 3 4 5 6 7 8 9)
;;   (2 3 4 5 6 7 8 9)         (3 4 5 6 7 8 9)  (1)
;;     (3 4 5 6 7 8 9)             (5 6 7 8 9)  (2 1)
;;       (4 5 6 7 8 9)                 (7 8 9)  (3 2 1)
;;         (5 6 7 8 9)                     (9)  (4 3 2 1)
;;           (6 7 8 9)                     (9)  (3 2 1)    (4 5)
;;             (7 8 9)                     (9)  (2 1)      (3 6 4 5)
;;               (8 9)                     (9)  (1)        (2 7 3 6 4 5)
;;                 (9)                     (9)  ()         (1 8 2 7 3 6 4 5)
;;                  ()                     (9)  ()         (9 1 8 2 7 3 6 4 5)

(defun alternate-high-low (lor)
  "Return a list of reals rearranged so alternate elements are each
greater than their two adjacent elements; in other words, the reals
are in alternating high-low order."
  ;; This implementation is O(nlogn) but walks the elements only 1 more time,
  ;; but it needs O(n/2) storage (reverse stack).
  ;; Since it's tail recursive, it could be tail call optimized, but since
  ;; this is not mandatory that a CL implementation performs TCO, we'll
  ;; rewrite it in iteration form.
  (if (cddr lor)
      (labels ((reoder (slow fast reversed result)
                 (cond
                   ((cdr fast)
                    (reoder (cdr slow) (cddr fast) (cons (car slow) reversed)
                            result))
                   (slow
                    (if reversed
                        (reoder (cdr slow) fast (cdr reversed)
                                (list* (car reversed) (car slow) result))
                        (cons (car slow) result)))
                   (fast
                    (list* (car fast) result))
                   (t result))))
        (let ((sorted (sort (copy-list lor) (function <))))
          (reoder sorted sorted '() '())))
      lor))


(defun alternate-high-low (lor)
  "Return a list of reals rearranged so alternate elements are each
greater than their two adjacent elements; in other words, the reals
are in alternating high-low order."
  ;; This implementation is O(nlogn) but walks the elements only 1 more time,
  ;; but it needs O(n/2) storage (reverse stack).
  (if (cddr lor)
      (let ((sorted (sort (copy-list lor) (function <))))
        (loop
          :with slow := sorted
          :with fast := sorted
          :with reversed := '()
          :with result   := '()
          :do (cond
                ((cdr fast)
                 (setf reversed (cons (car slow) reversed)
                       slow (cdr slow)
                       fast (cddr fast)))
                (slow
                 (if reversed
                     (setf result (list* (car reversed) (car slow) result)
                           slow (cdr slow)
                           reversed (cdr reversed))
                     (return (cons (car slow) result))))
                (fast
                 (return (list* (car fast) result)))
                (t (return result)))))
      lor))

(progn
 (test/even-before-odd)
 (test/reorder-alternating-elements)
 (test/zip)
 (test/alternate-high-low))


;; [/sourcecode]


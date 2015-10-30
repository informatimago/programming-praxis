
;; Two Swaps
;; October 23, 2015
;; 
;; Today we have one of those tricky interview questions that are easy
;; once you know the answer:
;; 
;;     You are given an array of three elements, in some random
;;     order. You must sort the array into increasing order, but may only
;;     use two swaps. How can you do it? 
;; 
;; Your task is to write a program that sorts a three-element array with
;; only two swaps. When you are finished, you are welcome to read or run
;; a suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

;; In Common Lisp; we use symbol-macrolet to define aliases to the places
;; in the vector:

[sourcecode lang="css"]
(defun two-swap-sort-vector-3 (v)
  (symbol-macrolet ((a (aref v 0))
                    (b (aref v 1))
                    (c (aref v 2)))
    (if (< a c)
        (if (< a b)
            (if (< b c)
                ;;   <
                ;;  < <
                ;; 1 2 3 --> 0
                nil
                ;;   <         <
                ;;  < >       < <
                ;; 1 3 2 --> 1 2 3
                (rotatef b c))
            ;;  <          <
            ;; > <        < <
            ;; 2 1 3 --> 1 2 3
            (rotatef a b))
        (if (< a b)
            ;;  >         <          <
            ;; < >       > <        < <
            ;; 2 3 1 --> 2 1 3 --> 1 2 3
            (progn (rotatef b c)
                   (rotatef a b))
            (if (< b c)
                ;;  >         <          <
                ;; > <       < >        < <
                ;; 3 1 2 --> 1 3 2 --> 1 2 3
                (progn (rotatef a b)
                       (rotatef b c))
                ;;   >         <
                ;;  > >       < <
                ;; 3 2 1 --> 1 2 3
                (rotatef a c))))
    v))
[/sourcecode]

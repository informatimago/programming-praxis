;; https://programmingpraxis.com/2018/08/17/matrix-search/

;; Matrix Search
;; August 17, 2018
;;
;; Today’s exercise is a simple task in matrix manipulation. Given a
;; matrix in which the rows are ordered but the columns or not,
;; efficiently search for a specific item in the matrix. For instance,
;; given the matrix
;;
;; 2 5 8
;; 1 4 7
;; 3 6 9
;;
;;
;; a search for 6 finds it in row 2 column 1, a search for 8 finds it in
;; row 0 column 2, and a search for 0 fails.
;;
;; Your task is to write a program that searches a matrix. When you are
;; finished, you are welcome to read or run a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.


;; [sourcecode lang="css"]

(defun matrix-search-1 (m x &key (test (function =)))
  (find x (make-array (reduce (function *) (array-dimensions m)) :displaced-to m)
        :test test))


;; Now, notice that: (log 1000000 2) = 19.931568
;; So if your matrices are smaller than 1 million elements, (eg. 1000 x 1000 or 100 x 10000),
;; you can stop here.




;; If we can't amortize pre-processing, then we'll have to search each rown independently.
;; The time complexity will be: O(rows*log₂(cols))

;; This is probably the expected answer by an interrogator
;; ( cf. https://www.youtube.com/watch?v=NA9B6-s6r7Y )
;; but notice how slower it is on small matrices!
;; (There's no right answer, there's the answer expected by the interrogator,
;; and the other answers).

(defun matrix-search-2 (m x &key (lessp (function <)))
  (loop
    :for row :below (array-dimension m 0)
    :do (multiple-value-bind (found col)
            (dichotomy (lambda (col)
                         (cond
                           ((funcall lessp x (aref m row col)) -1)
                           ((funcall lessp (aref m row col) x) +1)
                           (t                                   0)))
                       0 (array-dimension m 1))
          (when found
            (return-from matrix-search-2 (values (list row col)
                                                 (aref m row col))))))
  nil)




;; If we can pre-process the matrix, then the simpliest and most
;; efficient thing to do is to pre-compute all the results, which will
;; allows us to perform a dichotomic search

(defvar *matrix-search-3-cache* (cons nil nil))

(defun matrix-search-3 (m x &key (lessp (function <)))
  (let ((results (if (eql (car *matrix-search-3-cache*) m)
                     (cdr *matrix-search-3-cache*)
                     (setf (car *matrix-search-3-cache*) m
                           (cdr *matrix-search-3-cache*)
                           (let ((results (make-array (reduce (function *) (array-dimensions m))))
                                 (i -1))
                             (loop
                               :for r :below (array-dimension m 0)
                               :do (loop
                                     :for c :below (array-dimension m 1)
                                     :do (setf (aref results (incf i))
                                               (cons (aref m r c)
                                                     (list r c)))))
                             (sort results (function <) :key (function car)))))))
    (multiple-value-bind (found index)
        (dichotomy-search results x (lambda (a b)
                                      (cond
                                        ((funcall lessp a b) -1)
                                        ((funcall lessp b a) +1)
                                        (t                    0)))
                          :key (function car))
      (when found
        (values (cdr (aref results index))
                (car (aref results index)))))))



(defun gen (rows cols)
  (let ((p (* rows cols)))
    (make-array (list rows cols)
                :initial-contents
                (map 'list (lambda (row)
                             (sort (map 'list (function cdr) row)
                                   (function <)))
                     (group-by (sort (map 'vector (lambda (i) (cons (random p) i)) (iota p))
                                     (function <)
                                     :key (function car))
                               cols)))))


(defun benchmark ()
  (loop :for m :in (list
                    (gen 100 100)
                    #2A((0 4 10 12 15 17)
                        (1 11 16 19 21 22)
                        (2 5 6 7 8 9)
                        (3 13 14 18 20 23))
                    #2A((2 5 8)
                        (1 4 7)
                        (3 6 9)))
        :collect (cons (array-dimensions m)
                       (loop :for search :in '(matrix-search-1 matrix-search-2 matrix-search-3)
                             :collect (cons search
                                            (chrono-run-time (loop :for x :below 10000
                                                                   :do (funcall search m x))))))))


(benchmark)
;; --> (((100 100) (matrix-search-1 . 1.7939949999999953D0)   (matrix-search-2 . 0.8034000000006927D0)  (matrix-search-3 . 0.023526000000856584D0))
;;      ((4 6)    (matrix-search-1 . 0.013214000002335524D0) (matrix-search-2 . 0.04642099999909988D0) (matrix-search-3 . 0.01174199999877601D0))
;;      ((3 3)    (matrix-search-1 . 0.007996000000275671D0) (matrix-search-2 . 0.03040799999871524D0) (matrix-search-3 . 0.010618000000249594D0)))

;; So you can see that matrix-search-2 is the slowest in all cases!
;; and that matrix-search-1, the silliest O(rows*cols) implementation
;; is also the fastest for practical cases (small matrices), and if
;; you have big matrices and lots of searches, you better keep the
;; simple algorithm by performing a O(rows*cols*log(rows*cols))
;; precomputing to be able to find the results in O(log(rows*cols)).
;; And of course, if your matrices contain really the integers in
;; (iota (* rows cols)), then you can lookup the results in 1 access
;; just indexing the result vector which contains the coordinates of
;; the integer.

;; [/sourcecode]


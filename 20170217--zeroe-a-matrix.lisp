
;; Zeroing A Matrix
;; by programmingpraxis
;; Today's exercise is a simple interview question from ADP:
;; 
;; Given a two-dimensional matrix of integers, for any cell that
;; initially contains a zero, change all elements of that cell's row and
;; column to zero.


(defun zeroe-matrix (m)
  (let* ((nrows   (array-dimension m 0))
         (ncols   (array-dimension m 1))
         (rows    (make-array nrows :initial-element nil))
         (cols    (make-array ncols :initial-element nil)))
    ;; find columns and rows containing a zero:
    (dotimes (i nrows)
      (dotimes (j ncols)
        (when (zerop (aref m i j))
          (setf (aref cols j) t)
          (setf (aref rows i) t))))
    ;; zeroe rows:
    (dotimes (i nrows)
      (when (aref rows i)
        (dotimes (j ncols)
          (setf (aref m i j) 0))))
    ;; zeroe columns:
    (dotimes (j ncols)
      (when (aref cols j)
        (dotimes (i nrows)
          (setf (aref m i j) 0)))))
  m)


(defun test/zeroe-matrix ()

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 1))))
                  #2A((1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 1))))
                  #2A((1 1 0 1 1 1) (1 1 0 1 1 1) (0 0 0 0 0 0) (1 1 0 1 1 1) (1 1 0 1 1 1))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 0))))
                  #2A((1 1 0 1 1 0) (1 1 0 1 1 0) (0 0 0 0 0 0) (1 1 0 1 1 0) (0 0 0 0 0 0))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 0)
                                                                (1 1 1 1 1 1)
                                                                (1 1 1 1 1 0))))
                  #2A((1 1 0 1 1 0) (1 1 0 1 1 0) (0 0 0 0 0 0) (1 1 0 1 1 0) (0 0 0 0 0 0))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 0)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 0))))
                  #2A((1 1 0 1 1 0) (1 1 0 1 1 0) (0 0 0 0 0 0) (1 1 0 1 1 0) (0 0 0 0 0 0))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((1 1 1 1 1 1)
                                                                (1 1 1 1 1 1)
                                                                (1 1 0 1 1 0)
                                                                (1 1 1 1 1 0)
                                                                (1 1 0 1 1 0))))
                  #2A((1 1 0 1 1 0) (1 1 0 1 1 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))))

  (assert (equalp (zeroe-matrix (make-array '(5 6)
                                            :initial-contents '((0 1 1 1 1 1)
                                                                (1 1 1 1 1 0)
                                                                (1 1 0 1 1 1)
                                                                (1 1 1 1 0 1)
                                                                (1 0 1 1 1 1))))
                  #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))))

  :success)

(test/zeroe-matrix) ; --> :success





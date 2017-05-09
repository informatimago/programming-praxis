;;https://programmingpraxis.com/2017/05/09/distinct-characters/
;;
;; We've done similar exercises in the past:
;; 
;; Write a program to determine if all the characters in a string are
;; distinct. For instance, the word "Programming" has two m and two g, so
;; all the characters are not distinct, but the word "Praxis" has six
;; distinct characters.
;; 
;; Your task is to write a program to determine if all the characters in
;; a string are distinct; you should provide three solutions, one that
;; takes time O(n²), one that takes time O(n log n), and one that takes
;; time O(n). When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

(in-package "COMMON-LISP-USER")


(defun distinct-characters (string)

  #-cl-user::|distinct elements in O(n·logn)|
  (distinct-elements string)
  
  #+cl-user::|distinct elements in O(n·logn)|
  (distinct-ordered-elements string (function char<)))


(defun distinct-ordered-elements (sequence order &key
                                                   (start 0)
                                                   (end   nil)
                                                   (key   (function identity)))
  (cond
    ((listp sequence)
     (distinct-list-ordered-elements (nthcdr start sequence) order key 
                                     (and end (- end start))))
    ((vectorp sequence)
     (let ((end (or end (length sequence))))
       (if (and (zerop start) (= end (length sequence)))
           (distinct-vector-ordered-elements sequence order key )
           (distinct-vector-ordered-elements (make-array (- end start)
                                                         :element-type (array-element-type sequence)
                                                         :displaced-to sequence
                                                         :displaced-index-offset start)
                                             order key))))
    (t
     (error 'type-error :expected-type 'sequence :datum sequence))))


(defun distinct-list-ordered-elements (list order key count)

  #-cl-user::|distinct elements in O(n·logn)|
  (distinct-list-elements list key (lambda (a b)
                                     (not (or (funcall order a b)
                                              (funcall order b a))))
                          count)
  
  #+cl-user::|distinct elements in O(n·logn)|
  (loop
    :for current :on (sort (copy-list (if count
                                          (subseq list 0 count)
                                          list))
                           order :key key)
    :for entry := (first current)
    :for element := (funcall key entry)
    :when (or (null (rest current))
              (funcall order element (funcall key (second current)))) 
      :collect entry))


(defun distinct-vector-ordered-elements (vector order key)

  #-cl-user::|distinct elements in O(n·logn)|
  (distinct-vector-ordered-elements vector key (lambda (a b)
                                                 (not (or (funcall order a b)
                                                          (funcall order b a)))))

  #+cl-user::|distinct elements in O(n·logn)|
  (loop
    :with sorted := (sort vector order :key key)
    :for i :below (length sorted)
    :for entry := (aref sorted i)
    :for element := (funcall key entry)
    :when (or (<= (length sorted) (1+ i))
               (funcall order element (funcall key (aref sorted (1+ i)))))
      :collect entry))


(defun distinct-elements (sequence &key
                                     (start 0)
                                     (end   nil)
                                     (key   (function identity))
                                     (test  (function eql)))
  (cond
    ((listp sequence)
     (distinct-list-elements (nthcdr start sequence) key test
                             (and end (- end start))))
    ((vectorp sequence)
     (let ((end (or end (length sequence))))
       (if (and (zerop start) (= end (length sequence)))
           (distinct-vector-elements sequence key test)
           (distinct-vector-elements (make-array (- end start)
                                                 :element-type (array-element-type sequence)
                                                 :displaced-to sequence
                                                 :displaced-index-offset start)
                                     key test))))
    (t
     (error 'type-error :expected-type 'sequence :datum sequence))))


(defun distinct-list-elements (list key test count)

  #+cl-user::|distinct elements in O(n)|
  (macrolet ((loop-it (count)
               `(loop
                  ,@(when count (list :repeat count))
                  :with elements := (make-hash-table :test test)
                  :for entry :in list
                  :for element = (funcall key entry)
                  :do (setf (gethash element elements) element)
                  :finally (return elements))))
    (let ((distinct '()))
      (maphash (lambda (k e)
                 (declare (ignore k))
                 (push e distinct))
               (if count
                   (loop-it count)
                   (loop-it nil)))
      distinct))
  
  #+cl-user::|distinct elements in O(n·logn)|
  (declare (ignore list key test count))
  #+cl-user::|distinct elements in O(n·logn)|
  (error "Missing a lessp")
  
  #+cl-user::|distinct elements in O(n²)|
  (macrolet ((loop-it (count)
               `(loop
                  ,@(when count (list :repeat count))
                  :for current :on list
                  :for element := (first current)
                  :unless (member (funcall key element) (rest current) :test test)
                    :collect element)))
    (if count
        (loop-it count)
        (loop-it nil))))


(defun distinct-vector-elements (vector key test)

  #+cl-user::|distinct elements in O(n)|
  (loop
    :with elements := (make-hash-table :test test)
    :for entry :across vector
    :for element = (funcall key entry)
    :do (setf (gethash element elements) element)
    :finally (return (let ((distinct (make-array (hash-table-count elements)))
                           (i -1))
                       (maphash (lambda (k e)
                                  (declare (ignore k))
                                  (setf (aref distinct (incf i)) e))
                                elements)
                       distinct)))
  
  #+cl-user::|distinct elements in O(n·logn)|
  (declare (ignore vector key test))
  #+cl-user::|distinct elements in O(n·logn)|
  (error "Missing a lessp")
  
  #+cl-user::|distinct elements in O(n²)|
  (coerce (loop
            :for i :below (length vector)
            :for element := (aref vector i)
            :unless (find (funcall key element) vector :start (1+ i) :test test)
              :collect element) 'vector))


;; https://programmingpraxis.com/2017/02/24/nth-smallest-item-in-binary-search-tree/
;;
;; Nth Smallest Item In Binary Search Tree
;;
;; by programmingpraxis
;;
;; A binary search tree is a binary tree in which all nodes in the left
;; subtree are less than the current node and all nodes in the right
;; subtree are greater than the current node. Items arrive in random
;; order, are inserted into the binary search tree, and an in-order
;; traversal produces the items in ascending order.
;;
;; Your task is to write a program that finds the nth smallest item in a
;; binary search tree, without enumerating all of the items in the
;; tree. When you are finished, you are welcome to read or run a
;; suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.



(defstruct node
  label
  left
  right)

(defun sequence-to-tree (sequence lessp)
  (let ((v (sort (coerce sequence 'vector) lessp)))
    (labels ((to-tree (min max)
               (cond
                 ((> min max) nil)
                 ((= min max) (make-node :label (aref v min)))
                 (t           (let ((mid (truncate (+ min max) 2)))
                                (make-node :label (aref v mid)
                                           :left  (to-tree min (1- mid))
                                           :right (to-tree (1+ mid) max)))))))
      (to-tree 0 (1- (length v))))))

(defun nth-element (n tree)
  (labels ((search-nth (i node)
             (when (node-left node)
               (setf i (search-nth i (node-left node))))
             (when (= i n)
               (return-from nth-element node))
             (incf i)
             (when (node-right node)
               (setf i (search-nth i (node-right node))))
             i))
    (search-nth 0 tree)
    nil))

(loop with tree = (sequence-to-tree (loop for i below 15 collect i) (function <))
      for i below 16 for e = (nth-element i tree) collect (and e (node-label e)))
;; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 nil)



(loop with tree = (sequence-to-tree (loop for i below 10 collect i) (function <))
      for i below 16 for e = (nth-element i tree) collect (and e (node-label e)))
;; --> (0 1 2 3 4 5 6 7 8 9 nil nil nil nil nil nil)

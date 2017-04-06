;; https://programmingpraxis.com/2017/03/03/balanced-binary-search-trees/
;; https://www.linkedin.com/pulse/software-engineer-detained-several-hours-us-customs-given-fairchild
;;
;; The story exploded across the intertubes a few days ago: A software
;; engineer trying to enter the United States on a work visa was asked to
;; prove his occupation by writing a program for the Customs and Border
;; Protection agent:
;;
;; Write a function to check if a Binary Search Tree is balanced.
;;
;; Let’s help him.
;;
;; Your task is to write a function to check if a binary search tree is
;; balanced. When you are finished, you are welcome to read or run a
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


(defstruct node
  label
  left
  right)

(defgeneric balancedp (tree)
  (:documentation "Return nil if the tree is not balanced, or its number of nodes if it is.")
  (:method ((node t))    0)
  (:method ((node node))
    (let ((lc (balancedp (node-left  node)))
          (rc (balancedp (node-right node))))
      (and lc
           rc
           (<= (abs (- lc rc)) 1)
           (+ 1 lc rc)))))


(assert (balancedp nil))
(assert (balancedp (make-node)))
(assert (balancedp (make-node :left (make-node))))
(assert (not (balancedp (make-node :left (make-node :right (make-node))))))
(assert (balancedp (make-node :left (make-node :right (make-node))
                              :right (make-node :left (make-node)))))
(assert (not  (balancedp (make-node :left (make-node  :left (make-node)
                                                      :right (make-node))
                                    :right (make-node :left (make-node  :left (make-node)))))))



(let* ((left  (make-node :left  (make-node :left (make-node :left  (make-node)))))
       (right (make-node :right (make-node :left (make-node :right (make-node)))))
       (tree  (make-node :left left
                         :right right)))
  (format t ";; ~A is ~:[not ~;~]balanced.~%" 'left  (balancedp left))
  (format t ";; ~A is ~:[not ~;~]balanced.~%" 'right (balancedp right))
  (format t ";; ~A is ~:[not ~;~]balanced.~%" 'tree (balancedp tree)))

;; left is not balanced.
;; right is not balanced.
;; tree is not balanced.
nil

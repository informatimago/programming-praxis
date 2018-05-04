;; https://programmingpraxis.com/2017/12/08/deletion-from-a-cyclical-list/
;; Deletion From A Cyclical List
;; December 8, 2017
;; 
;; A cyclical list has no beginning and no end; Chez Scheme writes it
;; like this:
;; 
;; #0=(1 2 3 . #0#)
;; 
;; Your task is to write a program that removes an item from a cyclical
;; list; if the item is not present in the cyclical list, it should
;; remain unchanged. When you are finished, you are welcome to read or
;; run a suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.


(defun remove-from-list (element list)
  (multiple-value-bind (elements stem-length)
      (com.informatimago.common-lisp.cesarum.list:list-elements list)
    (let ((stem   (remove element (subseq elements 0 stem-length)))
          (circle (remove element (subseq elements stem-length))))
      (append stem (and circle (com.informatimago.common-lisp.cesarum.list:ensure-circular circle))))))




(defun remove-from-list (element list)
  (multiple-value-bind (elements stem-length)
      (com.informatimago.common-lisp.cesarum.list:list-elements list)
    (let ((stem   (remove element (subseq elements 0 stem-length)))
          (circle (remove element (subseq elements stem-length))))
      (append stem (and circle (com.informatimago.common-lisp.cesarum.list:ensure-circular circle))))))


(defun test/remove-from-list ()
 (loop :for (element list expected)
         :in '((blanc (bleu blanc blanc rouge . #1=(blanc rouge blanc bleu . #1#))
                (bleu rouge . #3=(rouge bleu . #3#)))
               (ball (ball ball ball . #2=(red ball yellow ball . #2#))
                #4=(red yellow . #4#))
               (ball (red ball yellow ball . #12=(ball ball ball . #12#))
                (red yellow))
               (ball #22=(ball ball ball . #22#)
                ())
               (foo #32=(ball ball ball . #32#)
                #33=(ball ball ball . #33#)))
       :do (assert (equal (multiple-value-list (com.informatimago.common-lisp.cesarum.list:list-elements
                                                (remove-from-list element list)))
                          (multiple-value-list (com.informatimago.common-lisp.cesarum.list:list-elements
                                                expected))))
       :finally (return :success)))


(test/remove-from-list)
:success




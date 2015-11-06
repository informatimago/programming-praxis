
;; Self-Organizing Lists
;; November 6, 2015
;; 
;; There are many applications where you need to keep a small number of
;; items, a set, and check for membership in the set, and there are many
;; algorithms for doing that: a linked list keeps the items in random
;; order to be retrieved by linear search, an ordered array keeps the
;; items in sequence to be retrieved by binary search, a hash table
;; performs some kind of math on the item, various kinds of trees can
;; store and search the items, and so on.
;; 
;; Today’s exercise looks at a simple algorithm that is appropriate for
;; search in a set that isn’t too big, doesn’t change too often, and
;; isn’t searched too many times, where the definition of “too” depends
;; on the needs of the user. The idea is to store the items of the set in
;; a linked list, search the list on request, and each time an item is
;; found, move it to the front of the list. The hope is that
;; frequently-accessed items will stay near the front of the list, so
;; that instead of an average search that requires inspection of half the
;; list, frequently-accessed items are found much more quickly.
;; 
;; Your task is to write functions that maintain a set as a
;; self-organizing list: you should provide functions to insert and
;; delete items from the set as well as to search within the set. When
;; you are finished, you are welcome to read or run a suggested solution,
;; or to post your own solution or discuss the exercise in the comments
;; below.


;; [sourcecode lang="css"]


#|

There is no list ADT in lisp, so it's not possible to mutate a list in
general (it's not possible to transform the symbol NIL which
represents the empty list into a CONS cell, or vice-versa,  a CONS
cell into the NIL symbol!).  We have two solutions to deal with
the mutation of list arguments:

1- introduce an ADT (eg. wrap the list in a structure),

2- use macros for the operations that can "mutate" a list into an
   empty list or vice-versa.

Since the option 1 is exemplified in the scheme solution, I'll
implement option 2.

INSERTF element list                                                Macro

    Inserts the ELEMENT into the place LIST, at the head.
    (This expands merely to a PUSH macro call).


DELETEF element list &key test key ...                              Macro

    Deletes the ELEMENT from the place LIST.
    (We use the obvious definition with all the &key arguments to the
    DELETE function)


SOL-FIND element list &key test key                                 Function

    Finds the designated ELEMENT in the LIST and returns it, while
    moving it into the first position of the LIST.  The LIST is
    mutated.  If the ELEMENT is found, a second value T is returned,
    otherwise two NIL values are returned. 
    
    Since this operation doesn't change the NULL status of the LIST
    argument, it can be implemented as a function mutating the LIST.
    The trick  is to keep the first CONS cell of the LIST in place,
    and to swap its CAR with the element found.


     list                 prev
       |                   |
       v                   v
     [a|*]-->[b|*]--> …  [p|*]-->[x|*]-->[s|*]-->

                               +-----------+
                               |           |
                               |           v
     [a|*]-+ [b|*]--> …  [p|*]-+ [x|*]-+ [s|*]--> 
           |   ^                  ^    |
           |   |                  |    |
           +---|------------------+    |
               +-----------------------+

|#


(defun sol-find (element list &key (test 'eql) (key 'identity))
  (if list
      (values (loop
                :for prev :on (cons nil list)
                :when (and (cdr prev) (funcall test element (funcall key (cadr prev))))
                  :do (unless (eq (cdr prev) list)
                        (rotatef (cddr prev) (cdr list) (cdr prev))
                        (rotatef (car list) (cadr list)))
                      (return (car list))
                :finally (return nil))
              t)
      (values nil nil)))


(defmacro insertf (list element)
  `(push ,element ,list))


;; We use define-modify-macro to define the DELETEF macro.
;; However, define-modify-macro expects that the modified place be the
;; first argument, so we have to swap the two first arguments of the
;; DELETE function:
(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))
(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

#|

(let ((list (list 1 2 3 4 5 6)))
  (values (sol-find 16 list :test (function =) :key (lambda (x) (* x x)))
          list))
--> 4
    (4 1 2 3 5 6)

(let ((list (list 1 2 3 4 5 6)))
  (values (sol-find 6 list)
          list))
--> 6
    (6 1 2 3 4 5)

(let ((list (list 1 2 3 4 5 6)))
  (values (sol-find 1 list)
          list))
--> 1
    (1 2 3 4 5 6)


(let ((list nil))
  (insertf list 5)
  (insertf list 4)
  (insertf list 3)
  (insertf list 2)
  (insertf list 1)
  (print list)
  (sol-find 3 list)
  (print list)
  (deletef list 2)
  (print list)
  (sol-find 5 list)
  (print list))

prints:
    (1 2 3 4 5) 
    (3 1 2 4 5) 
    (3 1 4 5) 
    (5 3 1 4)

--> (5 3 1 4)

|#

;; [/sourcecode]

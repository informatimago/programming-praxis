#|
https://programmingpraxis.com/2018/02/27/seven-segment-devices/


Seven-Segment Devices
February 27, 2018

We have today a simple exercise from Jon Bentley’s book Programming
Pearls, Chapter 3, Problem 8:

[S. C. Johnson] Seven-segment devices provide an inexpensive display
of the ten decimal digits:

 -----           -----   -----           -----   -----   -----   -----   -----
|     |       |       |       | |     | |       |             | |     | |     |
|     |       |       |       | |     | |       |             | |     | |     |
|     |       |       |       | |     | |       |             | |     | |     |
                 -----   -----   -----   -----   -----           -----   -----
|     |       | |             |       |       | |     |       | |     |       |
|     |       | |             |       |       | |     |       | |     |       |
|     |       | |             |       |       | |     |       | |     |       |
 -----           -----   -----           -----   -----           -----   -----
The seven segments are usually numbered as:

 --2--
|     |
3     4
|     |
 --1--
|     |
5     6
|     |
 --0--


Write a program that displays a 16-bit positive integer in five
seven-segment digits. The output is an array of five bytes; bit i of
byte j is one if and only if the ith segment of digit j should be on.

It was harder to type those digits than it looks.

Your task is to write a program to display numbers using seven-segment
digits, as Bentley directs. When you are finished, you are welcome to
read or run a suggested solution, or to post your own solution or
discuss the exercise in the comments below.


|#


[sourcecode lang="css"]

;; Let's have some fun with the format specfiers!

(defparameter *line-0* "~0@*~:[       ~; ----- ~]  ")
(defparameter *line-1* "~1@*~:[       ~; ----- ~]  ")
(defparameter *line-2* "~2@*~:[       ~; ----- ~]  ")
(defparameter *line-3* "~3@*~:[ ~;|~]     ")
(defparameter *line-4* "~4@*~:[ ~;|~]  ")
(defparameter *line-5* "~5@*~:[ ~;|~]     ")
(defparameter *line-6* "~6@*~:[ ~;|~]  ")


(defun format-digit-line (digit-count &rest segment-formats)
  (let ((*print-circle* nil))
    (format nil "~~~A:{~{~A~}~~:}~~%" digit-count segment-formats)))

(defconstant +digit-count+ 5)

(defparameter *segment-display* (concatenate 'string
                                             "~@*" (format-digit-line +digit-count+ *line-2*)
                                             "~@*" (format-digit-line +digit-count+ *line-3* *line-4*)
                                             "~@*" (format-digit-line +digit-count+ *line-3* *line-4*)
                                             "~@*" (format-digit-line +digit-count+ *line-3* *line-4*)
                                             "~@*" (format-digit-line +digit-count+ *line-1*)
                                             "~@*" (format-digit-line +digit-count+ *line-5* *line-6*)
                                             "~@*" (format-digit-line +digit-count+ *line-5* *line-6*)
                                             "~@*" (format-digit-line +digit-count+ *line-5* *line-6*)
                                             "~@*" (format-digit-line +digit-count+ *line-0*)))



(defun test/digits ()
 (let ((digit-count 9))
   (loop :for format-spec :across (vector
                                   (format-digit-line digit-count *line-2*)
                                   (format-digit-line digit-count *line-3* *line-4*)
                                   (format-digit-line digit-count *line-1*)
                                   (format-digit-line digit-count *line-5* *line-6*)
                                   (format-digit-line digit-count *line-0*))
         :do (format t format-spec
                     '((nil nil nil nil nil nil nil)
                       (t   nil nil nil nil nil nil)
                       (nil t   nil nil nil nil nil)
                       (nil nil t   nil nil nil nil)
                       (nil nil nil t   nil nil nil)
                       (nil nil nil nil t   nil nil)
                       (nil nil nil nil nil t   nil)
                       (nil nil nil nil nil nil t  )
                       (t   t   t   t   t   t   t  ))))))


(defun segments (digit)
  (case digit
    (0   '(t nil t      t t      t t))
    (1   '(nil nil nil  nil t    nil t))
    (2   '(t t t        nil t    t nil))
    (3   '(t t t        nil t    nil t))
    (4   '(nil t nil    t t      nil t))
    (5   '(t t t        t nil    nil t))
    (6   '(t t t        t nil    t t))
    (7   '(nil nil t    nil t    nil t))
    (8   '(t t t        t t      t t))
    (9   '(t t t        t t      nil t))
    (#xa '(nil t t      t t      t t))
    (#xb '(t t nil      t nil    t t))
    (#xc '(t t nil      nil nil  t nil))
    (#xd '(t t nil      nil t    t t))
    (#xe '(t t t        t nil    t nil))
    (#xf '(nil t t      t nil    t nil))))


(defun print-segments (n &key (base 10.))
  (check-type base (integer 2 16))
  (assert (<= n (1- (expt base +digit-count+)))
          (n) "n must be between 0 and ~A" (1- (expt base +digit-count+)))
  (let ((segments (nreverse (loop :repeat +digit-count+
                                  :for (rest digit) := (multiple-value-list (truncate n base))
                                    :then (multiple-value-list (truncate rest base))
                                  :collect (segments digit)))))
    (format t *segment-display* segments) (terpri)))




#|


cl-user> (print-segments 45067 :base 10)
          -----    -----    -----    -----   
|     |  |        |     |  |              |  
|     |  |        |     |  |              |  
|     |  |        |     |  |              |  
 -----    -----             -----            
      |        |  |     |  |     |        |  
      |        |  |     |  |     |        |  
      |        |  |     |  |     |        |  
          -----    -----    -----            

nil
cl-user> (print-segments #xb00b5 :base 16)
          -----    -----             -----   
|        |     |  |     |  |        |        
|        |     |  |     |  |        |        
|        |     |  |     |  |        |        
 -----                      -----    -----   
|     |  |     |  |     |  |     |        |  
|     |  |     |  |     |  |     |        |  
|     |  |     |  |     |  |     |        |  
 -----    -----    -----    -----    -----   

nil
cl-user>

|#

[/sourcecode]

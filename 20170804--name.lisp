;; Last Name Comma First Name
;; August 4, 2017
;; Today’s task is simple:
;; 
;; You are given a file containing one name per line in the format last
;; name comma first name, optionally followed by another comma and a
;; numeral (like Sr., or Jr., or IV), and are convert it to a file
;; containing the names, one name per line, in the format first name,
;; last name and numeral with no commas. You may assume the input is
;; correctly formatted, with optional spaces after each of the two
;; commas.
;; 
;; Your task is to convert a file of names to the correct format. When
;; you are finished, you are welcome to read a suggested solution, or to
;; post your own solution or discuss the exercise in the comments below.


(ql:quickload :com.informatimago.common-lisp.cesarum)
(ql:quickload :split-sequence)

(defun transform-name-file (path-with-comma path-without-comma)
  (setf (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents
         path-without-comma
         :if-does-not-exist :create
         :if-exists :supersede)
        (mapcar (lambda (line)
                  (destructuring-bind (name firstname &optional numeral)
                      (split-sequence:split-sequence #\, line)
                    (let ((name      (string-trim " " name))
                          (firstname (string-trim " " firstname))
                          (numeral   (and numeral (string-trim " " numeral))))
                      (format nil "~A ~A~@[ ~A~]" firstname name numeral))))
                (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents path-with-comma))))

(progn
  (transform-name-file "20170804--name.data" "20170804--name.txt")
  (values (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents "20170804--name.data")
          (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents "20170804--name.txt")))
;; --> ("McCarthy,John" "Bush,Georges,Jr." "Gates, Bill, III" "Newton, John")
;;     ("John McCarthy" "Georges Bush Jr." "Bill Gates III" "John Newton")


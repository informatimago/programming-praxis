(in-package "COMMON-LISP-USER")

(defun set-equal (a b) (and (subsetp a b) (subsetp b a)))

(loop
  :with options := '(cl-user::|distinct elements in O(n)|
                     cl-user::|distinct elements in O(n·logn)|
                     cl-user::|distinct elements in O(n²)|)
  :for option :in options
  :do (setf *features* (cons option (set-difference *features* options)))
      (load (compile-file (merge-pathnames #P"20170509--distinct-characters.lisp"
                                           (or *compile-file-pathname* *load-pathname* #P"./"))))
      (loop
        :for (test expected)
          :in (list (list (coerce "Praxis"      'list) '(#\P #\r #\a #\x #\i #\s))
                    (list (coerce "Programming" 'list) '(#\P #\r #\o #\g #\r #\a #\m #\i #\n))
                    (list "Praxis"                     '(#\P #\r #\a #\x #\i #\s))
                    (list "Programming"                '(#\P #\r #\o #\g #\r #\a #\m #\i #\n)))
        :for result := (distinct-characters test)
        :do (assert (set-equal (coerce result 'list) expected)
                    () "~%Tested:   ~S~%Expected: ~S~%Result:   ~S~2%"
                    test expected result)))

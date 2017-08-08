;; Move Spaces To Beginning Of String
;; August 8, 2017
;;
;; Today’s exercise is a programming interview question from a programmer
;; who very obviously didn’t get the job:
;;
;; Given a C-style null-terminated string, move all the spaces in the
;; string to the beginning of the string, in place, in one pass.
;;
;; The candidate claimed it couldn’t be done, and apparently got into an
;; argument with the interviewer, and took to the internet after the
;; interview to vindicate himself about the “unfair question.”
;;
;; Your task is to either write the requested program, or demonstrate
;; that it cannot be done. When you are finished, you are welcome to read
;; or run a suggested solution, or to post your own solution or discuss
;; the exercise in the comments below.

;; [sourcecode lang="css"]

(defun move-spaces-to-the-beginning (string)
  (replace string (stable-sort string (lambda (x y) (char= #\space x)))))

(let ((string (copy-seq "Hello World !")))
  (assert (eq string (move-spaces-to-the-beginning string)))
  string)
;; --> "  HelloWorld!"

;; [/sourcecode]

;; [sourcecode lang="css"]

(defun move-spaces-to-the-beginning (string)
  (loop
    :with d := (1- (length string))
    :for s :from d :downto 0
    :when (char/= #\space (aref string s))
      :do (loop :while (and (char/= #\space (aref string d)) (< s d))
                :do (decf d))
          (rotatef (aref string s) (aref string d))
    :finally (return string)))

(defun move-spaces-to-the-beginning (string)
  (loop
    :with d := (1- (length string))
    :for s :from d :downto 0
    :when (char/= #\space (aref string s))
      :do (setf (aref string d) (aref string s))
          (decf d)
    :finally (fill string #\space :start 0 :end (1+ d))
             (return string)))


(let ((string (copy-seq "FooBar")))
  (assert (eq string (move-spaces-to-the-beginning string)))
  string)



;; --> "FooBar"

(let ((string (copy-seq "Hello World !")))
  (assert (eq string (move-spaces-to-the-beginning string)))
  string)




;; --> "  HelloWorld!"

(let ((string (copy-seq "   Hello  World  !   ")))
  (assert (eq string (move-spaces-to-the-beginning string)))
  string)



;; --> "          HelloWorld!"

;; [/sourcecode]
